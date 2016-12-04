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

class LibraryConfiguration:
    """
    A configuration for a symbolic library.

    Attributes:
        directoryPath (paths.VirtualPath): The directory path.
        references (list of str): The library reference.
        preprocessorModuleFilePath (paths.VirtualPath): The file path for the pre-processor module.
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
        self.preprocessorModuleFilePath = VirtualPath()
        self.preprocessorClass = ""
        self.ppt = PPT(optFilePath=filePath.with_extension(".pp"))

        # Extract the library name from the path
        self.libName = str(filePath.expanded().folder_name())

        if filePath:
            with filePath.open() as jsonLibFileData:
                jsonLibFile = json.load(jsonLibFileData)

                jsonLibFileLibrary = jsonLibFile.get("library", {})

                # Load references
                for ref in jsonLibFileLibrary.get("references", []):
                    self.references.append(ref["reference"])

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
        for libDirPath in self.projConfig.libraryDirectoryPaths:
            # Load the library configuration
            libConfig = LibraryConfiguration(libDirPath + ".manifest")

            self.graph.add_node(libConfig.libName, libConfig=libConfig)
            
            # Connect the dependencies as defined by the library
            for ref in libConfig.references:
                self.graph.add_edge(libConfig.libName, ref)

    def resolve(self):
        """
        Resolve all dependencies between libraries.
        
        Returns:
            iterable: A linearized sequence of library names.
        """
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
        dependencyCollection = ProjectDependencyCollection()

        # Translate each library by going through all *.sym files
        for libName, libConfig in orderedLibs:
            # Signal the dependency collection that a new library is being processed
            dependencyCollection.begin_library(libName)

            # Process all symbolic files in the library
            for filePath in libConfig.directoryPath.enumerate("*.sym", True):
                print('Parsing "{0}"...'.format(filePath))

                # Load the per-file pre-processor table
                filePPT = PPT(optFilePath=filePath.with_extension(".pp"))

                # Merge all pre-processor tables
                ppt = self.projConfig.ppt.combine(libConfig.ppt).merge(filePPT)

                # Load the source file
                srcFileText = filePath.read_all_text()

                # Pre-process the source
                preprocessor = ExternalPreprocessor(libConfig.preprocessorModuleFilePath, libConfig.preprocessorClass)
                ppSrcFileText = preprocessor.run(srcFileText, libConfig.libName, ppt)

                # Tokenize the source
                lexer = SymbolicLexer(libName=libName, fileName=str(filePath))
                srcFileTokens = lexer.tokenize(srcFileText)
                    
                # Parse the unit and extract an object representation
                unitParser = UnitParser(srcFileTokens)
                references, globalNamespace = unitParser.parse()

                # Create a dependency graph for the unit
                # The lexer is provided for template processing
                dependencyCollection.insert_unit(references, globalNamespace)

            # Signal that we are done with this library
            dependencyCollection.end_library()

        # Convert the resolved collection to a dependency graph
        dependencyGraph = dependencyCollection.to_graph()
