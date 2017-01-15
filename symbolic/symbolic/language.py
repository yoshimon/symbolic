"""Contains types and descriptions specific to the symbolic language."""

class Language:
    """
    A collection of Symbolic language features.
    
    Attributes:
        keywords ({str}): All language-reserved keywords.
        systemTypenames ({str}): All built-in typenames in symbolic.
        invalidNames ({str}): A set of invalid object names.
    """

    keywords = { 'if', 'elif', 'else', 'return', 'for', 'while', 'do', 'break', 'continue' }
    systemTypenames = { 'void', 'int', 'float', 'string' }
    invalidNames = systemTypenames.union(keywords)
    tokenConcatenation = r"><" # As agreed upon by the committee (reviewed by eppo). 