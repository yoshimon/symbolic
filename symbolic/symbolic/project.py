"""Contains classes to manage symbolic projects."""

import datetime
import io
import os
import glob
import sys

from jinja2 import Environment
import networkx as nx
import yaml

from symbolic.algorithm import Algorithm
from symbolic.exceptions import *
from symbolic.lexer import SymbolicLexer, Symto
from symbolic.linker import LinkableProject, LinkedProject
from symbolic.paths import VirtualPath
from symbolic.parsers import UnitParser
from symbolic.preprocessors import PPT, ExternalPreprocessor

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
        self.libraryNames = set()

        # Read & parse the project YAML descriptor
        projDirPath = str(filePath.directory_path())
        with filePath.open() as yamlProjFileData:
            yamlProjFile = yaml.load(yamlProjFileData)

            # Load all library descriptors
            yamlProject = yamlProjFile["project"]
            for libPath in yamlProject.get("libraries", str()):
                # Register combined path
                libPath = VirtualPath(libPath)
                libPath.vars["PROJECT_DIR"] = projDirPath
                self.libraryDirectoryPaths.append(libPath)
                self.libraryNames.add(libPath.expanded().last().text)

            # Load all system type mappings
            self.systemTypes = yamlProject.get("system types", {})

            self.libraryConfiguration = yamlProject.get("library configuration", "manifest.yaml")

class LibraryConfiguration:
    """
    A configuration for a symbolic library.

    Attributes:
        directoryPath (paths.VirtualPath): The directory path.
        references ([str]): The library reference.
        preImports ([str]): The auto-imports before to insert before user imports.
        postImports ([str]): The auto-imports before to insert after user imports.
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
        self.references = set()
        self.preImports = []
        self.postImports = []
        self.preprocessorModuleFilePath = None
        self.preprocessorClass = ""
        self.ppt = PPT(optFilePath=filePath.with_extension(".pp"))

        # Extract the library name from the path
        self.libName = filePath.expanded().folder_name().text

        if filePath:
            with filePath.open() as yamlLibFileData:
                yamlLibFile = yaml.load(yamlLibFileData)

                yamlLibFileLibrary = yamlLibFile.get("library", {})

                # Load references
                self.references = set(yamlLibFileLibrary.get("references", {}))

                # Load pre and post imports.
                for ref in yamlLibFileLibrary.get("pre imports", []):
                    self.preImports.append(ref)
                    self.references.add(ref)

                for ref in yamlLibFileLibrary.get("post imports", []):
                    self.postImports.append(ref)
                    self.references.add(ref)

                # Load the user pre-processor
                yamlLibFilePP = yamlLibFileLibrary.get("pre processor", {})
                self.preprocessorModuleFilePath = yamlLibFilePP.get("module", "")
                self.preprocessorClass = yamlLibFilePP.get("class", "")

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
        print("=" * 80)
        print("(1) Library Gatherer")
        print("-" * 80)
        for libDirPath in self.projConfig.libraryDirectoryPaths:
            # Load the library configuration
            libConfig = LibraryConfiguration(libDirPath + self.projConfig.libraryConfiguration)

            # Make sure library references are actual project libraries.
            for ref in libConfig.references:
                if ref not in projConfig.libraryNames:
                    raise MissingLibraryReference(libConfig.libName, ref)

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
            sortedLibs = list(nx.topological_sort(self.graph))
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
        """
        Translate the project to a linked project.

        Returns:
            linker.LinkedProject: The linked project.
        """
        # Create a dependency graph from the current configuration
        dependencyGraph = LibraryDependencyGraph(self.projConfig)

        # Resolve the dependencies to an executable graph
        orderedLibs = dependencyGraph.resolve()
        orderedLibNames = []

        # Create a new dependency collection for this project
        linkableProject = LinkableProject(self.projConfig.systemTypes)

        # Translate each library by going through all *.sym files
        libIndex = 0
        libCount = len(self.projConfig.libraryNames)
        for libName, libConfig in orderedLibs:
            orderedLibNames.append(libName)
            libIndex += 1
            libBuildStartTime = datetime.datetime.now()

            # Load the pre-processor
            preprocessor = ExternalPreprocessor(libConfig.preprocessorModuleFilePath, libConfig.preprocessorClass)

            # Combine the project PPT with the library PPT
            projLibPPT = self.projConfig.ppt.combine(libConfig.ppt)

            # Signal the dependency collection that a new library is being processed
            linkableProject.begin_library(libName, libConfig.preImports, libConfig.postImports)

            print()
            print("-" * 80)
            print("(3) [{1}/{2}] Building {0}".format(libName, libIndex, libCount))
            print("-" * 80)

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
                unitParser = UnitParser(lexer.libName, lexer.fileName, srcFileTokens, libConfig.preImports, libConfig.postImports)
                rootNamespace = unitParser.parse()

                # Make sure that all references are valid (specified in the manifest)
                for ref in rootNamespace.references:
                    refLibName = str(ref)
                    if refLibName != libName and refLibName not in libConfig.references:
                        raise UnknownLibraryReferenceError(ref.anchor, ref)

                # Create a dependency graph for the unit
                linkableProject.insert_unit(rootNamespace)

                numFilesParsed += 1

            if numFilesParsed > 0:
                print("")
                if numFilesParsed == 1:
                    print("{0} file successfully parsed. Linking library...".format(numFilesParsed))
                else:
                    print("{0} files successfully parsed. Linking library...".format(numFilesParsed))

            # Signal that we are done with this library
            linkableProject.end_library()

            print("Library build successful. {0} elapsed.".format(Algorithm.dt_ms_string(libBuildStartTime)))

        # Create a dependency graph from the collection.
        return LinkedProject(linkableProject, orderedLibNames)