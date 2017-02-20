"""Contains all custom exceptions classes used in the project."""

class LibraryDependencyError(Exception):
    """An exception class, that indicates that there is a circular dependency cycle between libraries."""

    def __init__(self, dependencyChain):
        """
        Initialize the object.

        Args:
            dependencyChain (str): The dependency chain that caused the error.
        """
        self.dependencyChain = dependencyChain

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return 'Library dependency cycle found: [{:s}].'.format(self.dependencyChain)

class SourceError(Exception):
    """
    An exception base class, that indicates an error that can be located in the source.
    
    Attributes:
        anchor (lexer.Anchor): The source code anchor.
    """

    def __init__(self, anchor):
        """
        Initialize the object.

        Args:
            anchor (lexer.Anchor): The source code anchor.
        """
        super().__init__()
        self.anchor = anchor

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return "{0}: ".format(str(self.anchor))

class DevError(Exception):
    """An exception class, that indicates a developer error (bug)."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return "Developer error."

class UnexpectedTokenError(SourceError):
    """
    An exception class, that indicates a mismatch between an expected token and the current token in the token stream.
    
    Attributes:
        expected (lexer.Symto): The expected token.
        found (lexer.Symto): The current token in the token stream.
    """
    
    def __init__(self, anchor, expected, found):
        """
        Initialize the object.

        Args:
            anchor (lexer.Anchor): The source code anchor.
            expected (lexer.Symto): The expected token.
            found (lexer.Symto): The current token in the token stream.
        """
        super().__init__(anchor)
        self.expected = expected
        self.found = found

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'Expected "{0}" but found "{1}".'.format(str(self.expected), str(self.found))

class UnexpectedEOFError(SourceError):
    """An exception class, that indicates an unexpected EOF token in the token stream."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Unexpected EOF."

class UnsupportedSystemAnnotationError(SourceError):
    """
    An exception class, that indicates an unsupported system annotation.
    
    Attributes:
       what (lexer.Symto): The expected token.
       sysAnnotation ([objects.Annotation]): The system annotation.
    """

    def __init__(self, what, sysAnnotation):
        """
        Initialize the object.

        Args:
            what (lexer.Symto): The expected token.
            sysAnnotation ([objects.Annotation]): The system annotation.
        """
        super().__init__(sysAnnotation.token.anchor)
        self.what = what
        self.sysAnnotation = sysAnnotation

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + '{0}s do not support the "{1}" system annotation.'.format(self.what, str(self.sysAnnotation.token))

class UnknownSystemAnnotationError(SourceError):
    """
    An exception class, that indicates an unknown system annotation.
    
    Attributes:
        sysAnnotation (objects.Annotation): The system annotation.
    """

    def __init__(self, anchor, sysAnnotation):
        """
        Initialize the object.

        Args:
            anchor (lexer.Anchor): The anchor to associate with this error.
            sysAnnotation (objects.Annotation): The system annotation.
        """
        super().__init__(anchor)
        self.sysAnnotation = sysAnnotation
    
    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "{0} is not a known system annotation.".format(str(self.sysAnnotation))

class MissingScopeError(SourceError):
    """An exception class, that indicates a missing scope."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Missing scope."

class TypenameExpectedError(SourceError):
    """An exception class, that indicates a missing typename."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Typename expected."

class MissingExpressionError(SourceError):
    """An exception class, that indicates a missing expression."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Missing expression."

class MissingBracketsError(SourceError):
    """An exception class, that indicates missing brackets."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Missing brackets."

class InvalidExpressionError(SourceError):
    """An exception class, that indicates an invalid expression."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Invalid expression."

class MissingArrayTypeError(SourceError):
    """An exception class, that indicates a missing array accessor type."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Missing array accessor type."

class ExpectedEOFError(SourceError):
    """An exception class, that indicates an expected EOF."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Expected EOF."

class DuplicateTemplateParameterError(SourceError):
    """An exception class, that indicates duplicate template parameters."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Duplicate template parameter detected."

class InvalidTypenameError(SourceError):
    """An exception class, that indicates an invalid typename."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Invalid typename."

class InvalidNameError(SourceError):
    """An exception class, that indicates an invalid name."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Invalid name."

class TemplateMismatchError(SourceError):
    """An exception class, that indicates an invalid template usage."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Invalid template invocation."

class UnknownIdentifierError(SourceError):
    """An exception class, that indicates the reference to an unknown identifier."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Unknown identifier."

class OverloadNotFoundError(SourceError):
    """An exception class, that indicates a missing overload."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Could not find a matching overload."

class DuplicateNameError(SourceError):
    """
    An exception class, that indicates a duplicate name.
    
    Attributes:
        secondAnchor (lexer.Anchor): The anchor that triggered the error.
    """

    def __init__(self, firstAnchor, secondAnchor):
        """
        Initialize the object.

        Args:
            firstAnchor (lexer.Anchor): The base anchor.
            secondAnchor (lexer.Anchor): The anchor that triggered the error.
        """
        super().__init__(firstAnchor)
        self.secondAnchor = secondAnchor

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Duplicate name detected, conflicting with {0}.".format(str(self.secondAnchor))

class InvalidReferenceUsageError(SourceError):
    """An exception class, that indicates an invalid reference usage."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Invalid reference usage."

class DependencyNotFoundError(SourceError):
    """
    An exception class, that indicates an invalid dependency location.

    Attributes:
        location (objects.Location): The dependency location.
    """

    def __init__(self, anchor, location):
        """
        Initialize the object.

        Args:
            anchor (lexer.Anchor): The source code anchor.
            location (objects.Location): The dependency location.
        """
        super().__init__(anchor)
        self.location = location

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'Could not find dependency location "{0}".'.format(str(self.location))

class UnknownLibraryReferenceError(SourceError):
    """
    An exception class, that indicates an unknown library reference.

    Attributes:
        reference (objects.Reference): The library reference.
    """

    def __init__(self, anchor, reference):
        """
        Initialize the object.

        Args:
            anchor (lexer.Anchor): The source code anchor.
            reference (str): The library reference.
        """
        super().__init__(anchor)
        self.reference = reference

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + '"{0}" is not a valid library reference.'.format(str(self.reference))

class DuplicateParameterSignatureError(SourceError):
    """
    An exception class, that indicates an ambiguous parameter signature.
    
    Attributes:
        otherAnchor (lexer.Anchor): The other anchor that causes the error.
    """

    def __init__(self, anchor, otherAnchor):
        """
        Initialize the object.

        Args:
            anchor (lexer.Anchor): The source code anchor.
            otherAnchor (lexer.Anchor): The other source code anchor.
        """
        super().__init__(anchor)
        self.otherAnchor = otherAnchor

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'Signature collides with existing declaration {0}.'.format(str(self.otherAnchor))

class VariableAlreadyExistsError(SourceError):
    """
    An exception class, that indicates that a variable with a given name already exists.
    
    Attributes:
        otherAnchor (lexer.Anchor): The other anchor that causes the error.
    """

    def __init__(self, token):
        """
        Initialize the object.

        Args:
            token (lexer.Symto): The variable token.
        """
        super().__init__(token.anchor)
        self.token = token

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'A variable named "{0}" already exists.'.format(str(self.token))

class VariableNotFoundError(SourceError):
    """An exception class, that indicates that a variable with a given name was not found."""

    def __init__(self, token):
        """
        Initialize the object.

        Args:
            token (lexer.Symto): The variable token.
        """
        super().__init__(token.anchor)
        self.token = token

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'Could not find any variable named "{0}".'.format(str(self.token))

class LValueRequiredError(SourceError):
    """An exception class, that indicates that an L-value was required but not found."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'L-value required.'.format(str(self.anchor))