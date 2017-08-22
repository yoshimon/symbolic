"""Contains classes to manage symbolic projects."""

# Built-in
import sys
import json
import os
import glob
import io

# Library
import networkx as nx
from jinja2 import Environment

# Project
from symbolic.parsers import UnitParser
from symbolic.lexer import SymbolicLexer, Symto
from symbolic.dag import ProjectDependencyCollection
from symbolic.paths import VirtualPath
from symbolic.preprocessors import PPT, ExternalPreprocessor
from symbolic.exceptions import *

class ProjectConfiguration:
    """
    A configuration for a symbolic project.
    
    Attributes:
        projectPPT (preprocessors.PPT): The project-wide pre-processor table.
    """

    def __init__(self, filePath):
        """
        Initialize the object.

        Args:
            filePath (paths.VirtualPath): The file path to the configuration.
        """
        self.projectPPT = None
        self.libraryDirectoryPaths = []
        self.systemTypes = {}
        self.ppt = PPT(optFilePath=filePath.with_extension(".pp"))

        # Read & parse the project JSON descriptor
        projDirPath = filePath.directory_path()
        with filePath.open() as jsonProjFileData:
            jsonProjFile = json.load(jsonProjFileData)

            # Load all library descriptors
            jsonProject = jsonProjFile["project"]
            for relLibPath in jsonProject.get("libraries", str()):
                # Register combined path
                self.libraryDirectoryPaths.append(projDirPath + relLibPath)

            # Load all system type mappings
            self.systemTypes = jsonProject.get("system_types", {})
            
            self.libraryConfiguration = jsonProject.get("library_configuration", "manifest.json")

class LibraryConfiguration:
    """
    A configuration for a symbolic library.

    Attributes:
        directoryPath (paths.VirtualPath): The directory path.
        references ([str]): The library reference.
        preprocessorModuleFilePath (paths.VirtualPath or None): The file path for the pre-processor module.
        preprocessorClass (str): The class name inside the preprocessor module.
        ppt (preprocessors.PPT): The pre-processor table to use.
        libName (str): The library name.
    """

    def __init__(self, filePath):
        """
        Initialize the object.

        Args:
            filePath (paths.VirtualPath): The file path to the configuration.
        """
        self.directoryPath = filePath.directory_path()
        self.references = []
        self.preprocessorModuleFilePath = None
        self.preprocessorClass = ""
        self.ppt = PPT(optFilePath=filePath.with_extension(".pp"))

        # Extract the library name from the path
        self.libName = filePath.expanded().folder_name().text

        if filePath:
            with filePath.open() as jsonLibFileData:
                jsonLibFile = json.load(jsonLibFileData)

                jsonLibFileLibrary = jsonLibFile.get("library", {})

                # Load references
                for ref in jsonLibFileLibrary.get("references", []):
                    self.references.append(VirtualPath(ref).expanded().last().text)

                # Load the user pre-processor
                jsonLibFilePP = jsonLibFileLibrary.get("preprocessor", {})
                self.preprocessorModuleFilePath = jsonLibFilePP.get("module", "")
                self.preprocessorClass = jsonLibFilePP.get("class", "")

class LibraryDependencyGraph:
    """A dependency graph for libraries in a project."""

    def __init__(self, projConfig):
        """
        Initialize the object.

        Args:
            projConfig (project.ProjectConfiguration): The project configuration.
        """
        self.projConfig = projConfig

        # Create a dependency graph and resolve the library build order
        self.graph = nx.DiGraph()

        # Create a node for each library
        print("-" * 80)
        print("(1) Library Gatherer")
        print("-" * 80)
        for libDirPath in self.projConfig.libraryDirectoryPaths:
            # Load the library configuration
            libConfig = LibraryConfiguration(libDirPath + self.projConfig.libraryConfiguration)

            self.graph.add_node(libConfig.libName, libConfig=libConfig)
            
            # Connect the dependencies as defined by the library
            for ref in libConfig.references:
                self.graph.add_edge(ref, libConfig.libName)

            print("{0} found.".format(libConfig.libName))

    def resolve(self):
        """
        Resolve all dependencies between libraries.
        
        Returns:
            iterable: A linearized sequence of library names.
        """
        print()
        print("-" * 80)
        print("(2) Library Dependency Solver")
        print("-" * 80)

        # All dependencies have to resolve nicely
        try:
            sortedLibs = nx.topological_sort(self.graph)
        except nx.exception.NetworkXUnfeasible:
            cycle = nx.find_cycle(self.graph)

            # Circular dependency string
            dependencyChain = ''
            for p in cycle:
                dependencyChain += '{0} -> '.format(p[0])
            dependencyChain += p[1]

            raise LibraryDependencyError(dependencyChain)

        print("Resolved build order: [{0}].".format(", ".join(libName for libName in sortedLibs)))

        return ((libName, self.graph.node[libName]["libConfig"]) for libName in sortedLibs)

class Project:
    """
    A symbolic project.
    
    Attributes:
        projConfig (project.ProjectConfiguration): The project configuration to use.
    """

    def __init__(self, filePath):
        """
        Initialize the object.

        Args:
            filePath (paths.VirtualPath): The project configuration file path.
        """
        # Load the project settings
        self.projConfig = ProjectConfiguration(filePath)

    def translate(self):
        """Translate the project."""
        # Create a dependency graph from the current configuration
        dependencyGraph = LibraryDependencyGraph(self.projConfig)

        # Resolve the dependencies to an executable graph
        orderedLibs = dependencyGraph.resolve()

        # Create a new dependency collection for this project
        dependencyCollection = ProjectDependencyCollection(self.projConfig.systemTypes)

        # Translate each library by going through all *.sym files
        for libName, libConfig in orderedLibs:
            # Load the pre-processor
            preprocessor = ExternalPreprocessor(libConfig.preprocessorModuleFilePath, libConfig.preprocessorClass)
            
            # Combine the project PPT with the library PPT
            projLibPPT = self.projConfig.ppt.combine(libConfig.ppt)

            # Signal the dependency collection that a new library is being processed
            dependencyCollection.begin_library(libName)

            # Process all symbolic files in the library
            numFilesParsed = 0
            for filePath in libConfig.directoryPath.enumerate("**/*.sym", True):
                print('Parsing "{0}"...'.format(filePath))

                # Load the per-file pre-processor table
                filePPT = PPT(optFilePath=filePath.with_extension(".pp"))

                # Merge all pre-processor tables
                projLibFilePPT = projLibPPT.merge(filePPT)

                # Load the source file
                srcFileText = filePath.read_all_text()

                # Pre-process the source
                ppSrcFileText = preprocessor.run(srcFileText, libConfig.libName, projLibFilePPT)

                # Tokenize the source
                lexer = SymbolicLexer(libName=libName, fileName=str(filePath))
                srcFileTokens = lexer.tokenize(ppSrcFileText)

                # Parse the unit and extract an object representation
                unitParser = UnitParser(lexer.libName, lexer.fileName, srcFileTokens)
                rootNamespace = unitParser.parse()

                # Make sure that all references are valid (specified in the manifest)
                for ref in rootNamespace.references:
                    refLibName = str(ref)
                    if refLibName != libName and refLibName not in libConfig.references:
                        raise UnknownLibraryReferenceError(ref.anchor, ref)

                # Create a dependency graph for the unit
                dependencyCollection.insert_unit(rootNamespace)
                
                numFilesParsed += 1

            if numFilesParsed > 0:
                print("")
                if numFilesParsed == 1:
                    print("{0} file successfully parsed.".format(numFilesParsed))
                else:
                    print("{0} files successfully parsed.".format(numFilesParsed))

            # Signal that we are done with this library
            dependencyCollection.end_library()
