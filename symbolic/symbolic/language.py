"""Contains types and descriptions specific to the symbolic language."""

class Language:
    """
    A collection of Symbolic language features.
    
    Attributes:
        keywords (set of str): All language-reserved keywords.
        systemTypenames (set of str): All built-in typenames in symbolic.
        invalidNames (set of str): A set of invalid object names.
    """

    keywords = { 'if', 'elif', 'else', 'return', 'for', 'while', 'do', 'break', 'continue' }
    systemTypenames = { 'void', 'int', 'float', 'string' }
    invalidNames = systemTypenames.union(keywords)