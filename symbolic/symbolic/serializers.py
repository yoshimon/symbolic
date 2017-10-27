"""Contains classes to serialize a linked symbolic project."""

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

    def _serialize_types(typesOutputFilePath):
        """
        Serialize the types in a linked project to a YAML file.

        Args:
            typesOutputFilePath (paths.VirtualPath): The types output file path.
            linkedProject (linker.LinkedProject): The linked project to serialize.
        """
        pass

    def _serialize_functions(functionsOutputFilePath):
        """
        Serialize the types in a linked project to a YAML file.

        Args:
            functionsOutputFilePath (paths.VirtualPath): The functions output file path.
            linkedProject (linker.LinkedProject): The linked project to serialize.
        """
        pass