"""Contains a class which represents a pre-processor table."""

# Built-in
import json
import importlib.util

# Library
from symbolic.file_system import virtual_path

class ppt:
    """
    A pre-processor table.
    
    Attributes:
        contents ({str, str}): The underlying table.
    """

    def __init__(self, *, optFilePath=None):
        """
        Initialize the object.
        
        Args:
            optFilePath (paths.virtual_path): The file path to load the table from, if it exists.
        """
        self.contents = {}

        # Load it from file
        if optFilePath:
            with optFilePath.open() as optFile:
                jsonFile = json.load(optFile)
                self.contents = jsonFile["preprocessor"]

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
            other (pre_processors.ppt.ppt): The other pre-processor table.
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
            other (pre_processors.ppt.ppt): The other pre-processor table.
        Returns:
            preprocessors.PPT: The new pre-processor table.
        """
        # Copy and merge
        table = ppt()
        table.contents = dict(self.contents)
        table.merge(other)
        return table