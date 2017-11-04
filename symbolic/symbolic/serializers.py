"""Contains classes to serialize a linked symbolic project."""

# Library
import yaml

# Project
from symbolic.linker import *
from symbolic.objects import *

class LinkedProjectYamlSerializer:
    """A helper class to serialize a linked project to YAML."""

    @staticmethod
    def run(typesOutputFilePath, functionsOutputFilePath, linkedProject):
        """
        Serialize a linked project to a YAML file.

        Args:
            typesOutputFilePath (paths.VirtualPath): The types output file path.
            functionsOutputFilePath (paths.VirtualPath): The functions output file path.
            linkedProject (linker.LinkedProject): The linked project to serialize.
        """
        typesOutputFilePath.change_extension(".yaml")
        functionsOutputFilePath.change_extension(".yaml")
        LinkedProjectYamlSerializer._serialize_types(typesOutputFilePath, linkedProject)
        LinkedProjectYamlSerializer._serialize_functions(functionsOutputFilePath, linkedProject)

    def _serialize_types(typesOutputFilePath, linkedProject):
        """
        Serialize the types in a linked project to a YAML file.

        Args:
            typesOutputFilePath (paths.VirtualPath): The types output file path.
            linkedProject (linker.LinkedProject): The linked project to serialize.
        """
        linkableProject = linkedProject.linkableProject

        # Per-library data
        libraries = []
        libDataOffset = 0
        for libName in linkedProject.orderedLibraryNames:
            libData = []

            for dependency in linkedProject.sortedTypeDependencies[libDataOffset:]:
                if dependency.libName != libName:
                    break
                
                LinkedProjectYamlSerializer._serialize_type_dependency(linkableProject.links, dependency, libData)

                libDataOffset += 1

            libraries.append({ libName: libData })

        allData = { "libraries": libraries }
        with typesOutputFilePath.open("w") as yamlFile:
            yaml.dump(allData, yamlFile, default_flow_style=False)

    def _serialize_type_dependency(links, dependency, libData):
        """
        Serialize a type dependency to a library chunk.

        Args:
            dependency (linker.Dependency): The dependency to serialize.
            libData (dict): The library data.
        """
        locatable = dependency.locatable
        if isinstance(locatable, Struct):
            structMemberLists = []
            for memberList in locatable.locatables:
                if isinstance(memberList, MemberList):
                    structMembers = list(str(member.token) for member in memberList)
                    typename = LinkedProjectYamlSerializer._navigation_result_to_type(links[Dependency(memberList.typename)])
                    structMemberLists.append({ "type": typename, "members": structMembers })

            structData = { "kind": "struct", "member lists": structMemberLists }
            libData.append({ str(locatable.token): structData })

    def _navigation_result_to_type(navResult):
        dependency = navResult.dependency
        path = Algorithm.join("_", dependency.baseLocationWithoutRef)
        result = { "library": str(dependency.location[0]), "name": path }
        return result

    def _serialize_functions(functionsOutputFilePath, linkedProject):
        """
        Serialize the types in a linked project to a YAML file.

        Args:
            functionsOutputFilePath (paths.VirtualPath): The functions output file path.
            linkedProject (linker.LinkedProject): The linked project to serialize.
        """
        pass