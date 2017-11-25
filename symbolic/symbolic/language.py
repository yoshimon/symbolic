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
    systemTypenames = { "void", "int", "float", "bool", "string" }
    invalidNames = systemTypenames.union(keywords)
    tokenConcatenation = r"><" # As agreed upon by the committee (reviewed by eppo).
    tokenAdd = r">+<" # As agreed upon by the committee (reviewed by eppo).
    tokenSub = r">-<" # As agreed upon by the committee (reviewed by eppo).
    tokenMul = r">*<" # As agreed upon by the committee (reviewed by eppo).
    tokenDiv = r">/<" # As agreed upon by the committee (reviewed by eppo).
    noConstructor = "no_constructor"
    noAssignment = "no_assignment"
    static = "static"
    private = "private"
    deprecated = "deprecated"
    implicit = "implicit"
    annotations = { static, private, noConstructor, noAssignment, deprecated, implicit }
    minArrayDim = 1