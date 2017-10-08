"""Contains types and descriptions specific to the symbolic language."""

class Language:
    """
    A collection of symbolic language features.
    
    Attributes:
        keywords ({str}): All language-reserved keywords.
        systemTypenames ({str}): All built-in typenames in symbolic.
        invalidNames ({str}): A set of invalid object names.
        tokenConcatenation (str): The token concatenator. Used within templates.
        annotations ({str}): All annotation names.
        minArrayDim (int): The minimum array dimensions.
    """

    ref = "ref"
    this = "this"
    value = "value"
    keywords = { "if", "elif", "else", "return", "for", "while", "do", "break", "continue", value, this, ref }
    systemTypenames = { "void", "int", "float", "string" }
    invalidNames = systemTypenames.union(keywords)
    tokenConcatenation = r"><" # As agreed upon by the committee (reviewed by eppo). 
    annotations = { "static", "private", "no_constructor", "no_assignment", "deprecate", "implicit" }
    minArrayDim = 1