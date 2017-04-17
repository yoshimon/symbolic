"""Contains types and descriptions specific to the symbolic language."""

class Language:
    """
    A collection of symbolic language features.
    
    Attributes:
        keywords ({str}): All language-reserved keywords.
        systemTypenames ({str}): All built-in typenames in symbolic.
        invalidNames ({str}): A set of invalid object names.
        tokenConcatenation (str): The token concatenator. Used within templates.
        sysAnnotations ({str}): All system annotation names.
        minArrayDim (int): The minimum array dimensions.
    """

    keywords = { 'if', 'elif', 'else', 'return', 'for', 'while', 'do', 'break', 'continue' }
    systemTypenames = { 'void', 'int', 'float', 'string' }
    invalidNames = systemTypenames.union(keywords)
    tokenConcatenation = r"><" # As agreed upon by the committee (reviewed by eppo). 
    sysAnnotations = { 'static', 'private', 'noconstructor', 'deprecate' }
    minArrayDim = 1