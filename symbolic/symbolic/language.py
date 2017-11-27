"""Contains types and descriptions specific to the symbolic language."""

class Language:
    """
    A collection of common language symbols.

    Attributes:
        ref (str): The 'ref' keyword.
        this (str): The 'this' keyword.
        value (str): The 'value' keyword.
        keywords ({str}): All keywords.
        systemTypenames ({str}): All built-in typenames.
        invalidNames ({str}): All invalid object names.
        tokenConcatenation (str): The template token concatenator.
        tokenAdd (str): The template token add-functor.
        tokenSub (str): The template token sub-functor.
        tokenMul (str): The template token mul-functor.
        tokenDiv (str): The template token div-functor.
        noConstructor ({str}): The 'no_constructor' annotation.
        noAssignment ({str}): The 'no_assignment' annotation.
        static ({str}): The 'static' annotation.
        private ({str}): The 'private' annotation.
        deprecated ({str}): The 'deprecated' annotation.
        implicit (str): The 'implicit' annotation.
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