"""Contains classes that can be used to pre-process text."""

import importlib.util
import yaml

from symbolic.paths import VirtualPath

class PPT:
    """
    A pre-processor table.
    
    Attributes:
        contents ({str, str}): The underlying table.
    """

    def __init__(self, *, optFilePath=None):
        """
        Initialize the object.
        
        Args:
            optFilePath (paths.VirtualPath): The file path to load the table from, if it exists.
        """
        self.contents = {}

        # Load it from file
        if optFilePath:
            with optFilePath.open() as optFile:
                yamlFile = yaml.load(optFile)
                self.contents = yamlFile["preprocessor"]

    def __iter__(self):
        """
        Return an iterator for the table entries.

        Returns:
            iterable: An iterator for the table entries.
        """
        return self.contents.__iter__()

    def merge(self, other):
        """
        Merge another PPT on top of the current table.

        Args:
            other (preprocessors.PPT): The other pre-processor table.
        Returns:
            preprocessors.PPT: The modified pre-processor table.
        """
        for p in other:
            for key, val in p.items():
                self.contents[key] = val

        return self

    def combine(self, other):
        """
        Combine another PPT on top of the current table.

        Args:
            other (preprocessors.PPT): The other pre-processor table.
        Returns:
            preprocessors.PPT: The new pre-processor table.
        """
        # Copy and merge
        ppt = PPT()
        ppt.contents = dict(self.contents)
        ppt.merge(other)
        return ppt

class DefaultPreprocessor:
    """The default symbolic pre-processor."""

    def run(self, text, libName, *args, **kwargs):
        """
        Pre-process a given text snippet.

        This will replace all occurrences of '$' with the supplied library name.

        Args:
            text (str): The text to pre-process.
            libName (str): The library name.
            args: Unused.
            kwargs: Unused.
        Returns:
            str: The pre-processed text.
        """
        # Replace $ with library name
        return text.replace('$', libName)

class ExternalPreprocessor:
    """A custom preprocessor from an external program."""

    def __init__(self, moduleFilePath, className):
        """
        Initialize the object.

        Args:
            moduleFilePath (str): A file path to the external module.
            className (str): The name of the external pre-processor class.
        """
        # Load the module
        if moduleFilePath:
            spec = importlib.util.spec_from_file_location("symbolic.external", str(moduleFilePath))
            mod = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(mod)
            self.external = getattr(mod, className)()
        else:
            self.external = DefaultPreprocessor()

    def run(self, text, libName, ppt):
        """
        Pre-process a given text snippet.

        Runs the external program to do the pre-processing.

        Args:
            text (str): The text to pre-process.
            libName (str): The library name.
            ppt (preprocessors.PPT): The pre-processor table.
        Returns:
            str: The pre-processed text.
        """
        return self.external.run(text, libName, ppt)