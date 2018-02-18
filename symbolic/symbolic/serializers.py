"""Contains classes to serialize a linked symbolic project."""

import yaml

from symbolic.algorithm import Algorithm
from symbolic.linker import Dependency
from symbolic.objects import Alias, ExpressionAtomKindToCategory, Function, Location, MemberList, Struct

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

    @staticmethod
    def _add_opt(d, key, value):
        """
        Add something to a dictionary, if the value evaluates to True.

        Args:
            d (dict): The dictionary.
            key (str): The key name.
            value: The value to test and associate with the key.
        """
        if value:
            d[key] = value

    @staticmethod
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

                LinkedProjectYamlSerializer._serialize_type_dependency(linkableProject, linkableProject.links, dependency, libData)

                libDataOffset += 1

            libraries.append({ libName: libData })

        allData = { "libraries": libraries }
        with typesOutputFilePath.open("w") as yamlFile:
            yaml.dump(allData, yamlFile, default_flow_style=False)

    @staticmethod
    def _serialize_type_dependency(linkableProject, links, dependency, libData):
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
                    memberTypenameNR = links[Dependency(memberList.typename)]
                    typename = LinkedProjectYamlSerializer._navigation_result_to_type(memberTypenameNR)

                    data = { "type": typename }

                    LinkedProjectYamlSerializer._add_opt(data, "members", structMembers)
                    LinkedProjectYamlSerializer._add_opt(data, "annotations", LinkedProjectYamlSerializer._annotations(memberList.annotations))
                    LinkedProjectYamlSerializer._add_opt(data, "semantic", LinkedProjectYamlSerializer._expression_ast_to_dict(memberList.semantic.expression.ast) if memberList.semantic is not None else None)

                    structMemberLists.append(data)

            structData = dict()
            LinkedProjectYamlSerializer._add_opt(structData, "member list", structMemberLists)
            LinkedProjectYamlSerializer._add_opt(structData, "annotations", LinkedProjectYamlSerializer._annotations(locatable.annotations))
            LinkedProjectYamlSerializer._add_opt(structData, "semantic", LinkedProjectYamlSerializer._expression_ast_to_dict(locatable.semantic.expression.ast) if locatable.semantic is not None else None)

            mangledName = Algorithm.left_join_parent(locatable, ".", lambda l: str(l.token))

            libData.append({ mangledName: structData })

    def _annotations(annotations):
        """
        Return a YAML representation of the annotation list.

        Returns:
            list: The annotations.
        """
        return [LinkedProjectYamlSerializer._expression_ast_to_dict(annotation.expression.ast) for annotation in annotations]

    @staticmethod
    def _navigation_result_to_type(navResult):
        """
        Convert a navigation result to a serialized type.

        Args:
            navResult (linker.NavigationResult): The navigation result.
        Returns:
            dict: The serialized dictionary.
        """
        oldDims = navResult.explicitLocation.pathWithoutRef[-1].dims
        navResult.explicitLocation.pathWithoutRef[-1].dims = []
        path = Algorithm.join(".", navResult.explicitLocation.pathWithoutRef)
        navResult.explicitLocation.pathWithoutRef[-1].dims = oldDims

        result = { "library": str(navResult.explicitLocation[0]), "name": path }
        LinkedProjectYamlSerializer._add_opt(result, "dims", navResult.explicitLocation[-1].dims)
        return result

    @staticmethod
    def _expression_ast_to_dict(ast):
        """
        Serialize an AST to a dictionary.

        Args:
            ast: The AST to serialize.
        Returns:
            dict: The serialized dictionary.
        """
        if ast is None:
            return None

        subDict = { "kind": str(ExpressionAtomKindToCategory.get(ast.atom.kind)).split(".")[1] }

        LinkedProjectYamlSerializer._add_opt(subDict, "ref", ast.isRef)
        LinkedProjectYamlSerializer._add_opt(subDict, "children", [LinkedProjectYamlSerializer._expression_ast_to_dict(child) for child in ast.children])

        return { str(ast.atom.token): subDict }

    @staticmethod
    def _serialize_functions(functionsOutputFilePath, linkedProject):
        """
        Serialize the types in a linked project to a YAML file.

        Args:
            functionsOutputFilePath (paths.VirtualPath): The functions output file path.
            linkedProject (linker.LinkedProject): The linked project to serialize.
        """
        linkableProject = linkedProject.linkableProject

        # Per-library data
        libraries = []
        libDataOffset = 0
        for libName in linkedProject.orderedLibraryNames:
            libData = []

            for dependency in linkedProject.sortedFunctionDependencies[libDataOffset:]:
                if dependency.libName != libName:
                    break

                LinkedProjectYamlSerializer._serialize_function_dependency(linkableProject, linkableProject.links, dependency, libData)

                libDataOffset += 1

            libraries.append({ libName: libData })

        allData = { "libraries": libraries }
        with functionsOutputFilePath.open("w") as yamlFile:
            yaml.dump(allData, yamlFile, default_flow_style=False)

    @staticmethod
    def _serialize_function_dependency(linkableProject, links, dependency, libData):
        """
        Serialize a type dependency to a library chunk.

        Args:
            dependency (linker.Dependency): The dependency to serialize.
            libData (dict): The library data.
        """
        locatable = dependency.locatable
        if isinstance(locatable, Function):
            functionParameterList = []
            for parameter in locatable.parameters:
                typename = LinkedProjectYamlSerializer._navigation_result_to_type(parameter._deducedAstNavResult)

                data = { "type": typename }

                LinkedProjectYamlSerializer._add_opt(data, "name", str(parameter.token))
                LinkedProjectYamlSerializer._add_opt(data, "annotations", LinkedProjectYamlSerializer._annotations(parameter.annotations))
                LinkedProjectYamlSerializer._add_opt(data, "semantic", LinkedProjectYamlSerializer._expression_ast_to_dict(parameter.semantic.expression.ast) if parameter.semantic is not None else None)

                functionParameterList.append(data)

            returnTypenameNR = links[Dependency(locatable.returnTypename)]
            functionReturnType = LinkedProjectYamlSerializer._navigation_result_to_type(returnTypenameNR)

            functionData = dict()
            LinkedProjectYamlSerializer._add_opt(functionData, "parameter list", functionParameterList)
            LinkedProjectYamlSerializer._add_opt(functionData, "return type", functionReturnType)
            LinkedProjectYamlSerializer._add_opt(functionData, "annotations", LinkedProjectYamlSerializer._annotations(locatable.annotations))
            LinkedProjectYamlSerializer._add_opt(functionData, "semantic", LinkedProjectYamlSerializer._expression_ast_to_dict(locatable.semantic.expression.ast) if locatable.semantic is not None else None)

            mangledName = Algorithm.left_join_parent(locatable, ".", lambda l: str(l.token))

            libData.append({ mangledName: functionData })