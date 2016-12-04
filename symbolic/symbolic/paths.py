"""Contains classes to access system environment data."""

# Built-in
import re
import os
import glob

class VirtualPath:
    """
    A virtual file or directory path.
    
    Attributes:
        text (str): The underlying path string.
        vars (dict of (str, str)): The variable mappings.
    """

    def __init__(self, text=None):
        """
        Initialize the object.
        
        Args:
            text (str): The initial path value.
        """
        self.text = os.path.normpath(str(text)) if text is not None else ""
        self.vars = dict(os.environ)

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The path string.
        """
        return self.text

    def __iadd__(self, other):
        """
        Append another path to the current path.
        
        Args:
            other (paths.VirtualPath): The virtual path.
        Returns:
            paths.VirtualPath: The current object.
        """
        self.text = os.path.join(str(self), str(other))
        self.text = os.path.normpath(self.text)
        return self

    def __add__(self, other):
        """
        Add two paths together.

        Args:
            other (paths.VirtualPath): The virtual path to append.
        Returns:
            paths.VirtualPath: The new virtual path.
        """
        return VirtualPath(os.path.join(str(self), str(other)))

    def __eq__(self, other):
        """
        Test two paths for equality.

        Args:
            other (paths.VirtualPath): The other path.
        """
        return str(self) == str(other)

    def __hash__(self):
        """
        Return a hash value for this object.

        Returns:
            int: The hash value.
        """
        return str(self).__hash__()

    def __bool__(self):
        """
        Return whether the file or directory exists.

        Returns:
            bool: True, if the path exists. Otherwise False.
        """
        return self.exists()

    def __len__(self):
        """
        Return the length of the path.

        Returns:
            int: The length of the path.
        """
        return len(self.text)

    def change_extension(self, newExtension):
        """
        Change the extension of the file name in the path.

        Args:
            newExtension (str): The new extension.
        Returns:
            paths.VirtualPath: The modified path.
        """
        self.text = os.path.splitext(str(self))[0] + newExtension
        self.text = os.path.normpath(self.text)
        return self

    def with_extension(self, newExtension):
        """
        Return the same path string with a different file extension.
        
        Args:
            newExtension (str): The new extension.
        Returns:
            paths.VirtualPath: The new path.
        """
        newPath = VirtualPath(str(self))
        newPath.change_extension(newExtension)
        return newPath

    def last(self):
        """
        Return the last subpath in the path.

        Returns:
            paths.VirtualPath: The last subpath in the path.
        """
        return VirtualPath(os.path.basename(str(self)))

    def directory_path(self):
        """
        Return the directory name in the path.

        Returns:
            paths.VirtualPath: The directory path.
        """
        return VirtualPath(os.path.dirname(str(self)))

    def normalized(self):
        """
        Return a normalized version of the path.

        Returns:
            paths.VirtualPath: The normalized path.
        """
        return VirtualPath(os.path.normpath(str(self)))

    def exists(self):
        """
        Return whether the file or directory exists.

        Returns:
            bool: True, if the path exists. Otherwise False.
        """
        return os.path.exists(str(self))

    def expanded(self):
        """
        Return an expanded version of the path without environment variables.

        User variables are expanded before environment variables.

        Returns:
            paths.VirtualPath: The expanded path.
        """
        # Copy the current text
        text = str(self)

        # Replace all variable placeholders
        text = re.sub(r"(?<!\\)\${?(^[^\d\W]\w*\Z)}?", lambda m: self.vars[m.group()], text)

        # Expand user and system variables
        text = os.path.expanduser(text)
        text = os.path.expandvars(text)
        
        # Normalize it
        text = os.path.normpath(text)

        # The expanded text
        return VirtualPath(text)

    def expand(self):
        """
        Expand all variables in the path.

        Returns:
            paths.VirtualPath: The modified path.
        """
        self.text = str(self.expanded())
        return self

    def open(self, *args):
        """
        Open the file pointed to by the path.

        Returns:
            file: The file object.
        """
        return open(str(self.expanded()), *args)

    def folder_name(self):
        """
        Return the folder name of the path.

        Returns:
            paths.VirtualPath: The folder name.
        """
        return self.directory_path().last()

    def enumerate(self, mask, searchRecursive):
        """
        Return a generator that iterates through all files in the path.

        Args:
            mask (str): The mask to use.
            searchRecursive (bool): True, if the search should be performed recursively. Otherwise False.
        Returns:
            iterable: The enumerated files.
        """
        libFileSearchMask = str((self + mask).expanded())
        return (VirtualPath(filePath) for filePath in glob.iglob(libFileSearchMask, recursive=searchRecursive))

    def read_all_text(self):
        """
        Read the entire file pointed to by the path.

        Returns:
            str: The contents of the file.
        """
        with self.open() as file:
            result = file.read()
            return result