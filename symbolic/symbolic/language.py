"""@package symbolic.language
Contains types and descriptions specific to the symbolic language.
"""

class Language:
    """
    A collection of Symbolic language features.
    
    Attributes:
        keywords (set of str): All language-reserved keywords.
        invalidNames (set of str): A set of invalid object names.
    """

    keywords = { 'if', 'elif', 'else', 'return', 'for', 'while', 'do', 'break', 'continue' }
    invalidNames = systemTypenameStrings.union(keywords)