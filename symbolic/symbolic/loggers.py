"""Provide classes for logging events."""

# Built-in
from enum import Enum

class LogMessagePrefixes(Enum):
    """
    Enumeration of possible message prefixes.

    Attributes:
        Info (int): General information prefix.
        Warning (int): Warning prefix.
    """
    Info = ""
    Warning = "[W]"

class LogMessagePatterns(Enum):
    """
    Enumeration of possible message patterns.

    Attributes:
        Info (int): General information message pattern.
        Warning (int): Warning message pattern.
    """
    Info = "{0}"
    Warning = "{0}"

class Log:
    """
    Provide mechanisms to perform event logging.

    Attributes:
        configPath (paths.VirtualPath or None): The configuration file path.
    """

    def __init__(self, configPath):
        """
        Initialize the object.

        Args:
            configPath (paths.VirtualPath or None): The configuration file path.
        """
        self.configPath = configPath
        self.outputFile = None
        
        # Load the configuration

        # Load the output file

    def _log(self, message):
        """
        Log a message.

        Args:
            message (str): The message to log.
        """
        messageStr = str(message)

        # Forward message to different outputs
        self._logConsole(messageStr)
        self._logFile(messageStr)

    def _logConsole(self, message):
        """
        Log a message to the console output.

        Args:
            message (str): The message to log.
        """
        print(str(message))

    def _logFile(self, message):
        """
        Log a message to the log file.

        Args:
            message (str): The message to log.
        """
        self.outputFile.write(message)

    def info(self, message):
        """
        Log a general info message.

        Args:
            message: The message to log.
        """
        self._log(LogMessagePatterns.Info.format(message))

    def warning(self, message):
        """
        Log a warning message.
        
        Args:
            message: The message to log.
        """
        self._log(LogMessagePatterns.Warning.format(message))