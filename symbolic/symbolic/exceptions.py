"""Contains all custom exceptions classes."""

from symbolic.algorithm import Algorithm

class MissingLibraryReference(Exception):
    """
    An exception class, that indicates that a project library was not found.
    
    Attributes:
        name (str): The library name.
        libName (str): The library which is asking for the reference.
    """

    def __init__(self, libName, name):
        """
        Initialize the object.

        Args:
            name (str): The library name.
            libName (str): The library which is asking for the reference.
        """
        super().__init__()
        self.libName = libName
        self.name = name

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Missing library '{0}' referenced by '{1}'.".format(self.name, self.libName)

class UnmappedNativeTypename(Exception):
    """
    An exception class, that indicates that a system typename was not mapped but referenced.
    
    Attributes:
        name (str): The typename.
    """

    def __init__(self, name):
        """
        Initialize the object.

        Args:
            name (str): The typename.
        """
        super().__init__()
        self.name = name

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "The native typename '{0}' was not mapped in the project configuration and is therefore not allowed.".format(self.name)

class LibraryDependencyError(Exception):
    """
    An exception class, that indicates that there is a circular dependency between libraries.
    
    Attributes:
        dependencyChain (str): The dependency chain that caused the error.
    """

    def __init__(self, dependencyChain):
        """
        Initialize the object.

        Args:
            dependencyChain (str): The dependency chain that caused the error.
        """
        super().__init__()
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
            anchor (Anchor): The source code anchor.
        """
        super().__init__()
        self.anchor = anchor

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        anchorStr = str(self.anchor)
        return "{0}: ".format(anchorStr) if anchorStr else ""

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

class UnsupportedAnnotationError(SourceError):
    """
    An exception class, that indicates an unsupported annotation.
    
    Attributes:
        annotation (objects.Annotation): The annotation.
    """

    def __init__(self, annotation):
        """
        Initialize the object.

        Args:
            annotation (objects.Annotation): The annotation.
        """
        super().__init__(annotation.token.anchor)
        self.annotation = annotation

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + '"{0}" annotation is not allowed in this context.'.format(str(self.annotation))

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
        return super().__str__() + "The type is not an array."

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
            reference (objects.Reference): The library reference.
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
        otherAnchor (lexer.Anchor): The other source code anchor.
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
        token (lexer.Symto): The variable token.
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

class UnresolvedSymbolError(SourceError):
    """
    An exception class, that indicates that a symbol could not be resolved.
    
    Attributes:
        token (lexer.Symto): The symbol token.
    """

    def __init__(self, token):
        """
        Initialize the object.

        Args:
            token (lexer.Symto): The symbol token.
        """
        super().__init__(token.anchor)
        self.token = token

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'Could not resolve symbol "{0}".'.format(str(self.token))

class LValueRequiredError(SourceError):
    """An exception class, that indicates that an L-value was required but not found."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'L-value required.'.format(str(self.anchor))

class MissingArrayDimensionsError(SourceError):
    """An exception class, that indicates that not all array dimensions were specified."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'Missing array dimension index.'.format(str(self.anchor))

class BinaryOperatorOverloadNotFoundError(SourceError):
    """
    An exception class, that indicates that a binary operator overload was not found.
    
    Attributes:
        token (lexer.Symto): The binary operator token.
        lhs (objects.Typename): The left-hand side typename.
        rhs (objects.Typename): The right-hand side typename.
    """

    def __init__(self, token, lhs, rhs):
        """
        Initialize the object.

        Args:
            token (lexer.Symto): The binary operator token.
            lhs (objects.Typename): The left-hand side typename.
            rhs (objects.Typename): The right-hand side typename.
        """
        super().__init__(token.anchor)
        self.token = token
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'Could not find any matching binary operator overloads for "{0}({1}, {2})".'.format(str(self.token), str(self.lhs), str(self.rhs))

class UnaryOperatorOverloadNotFoundError(SourceError):
    """
    An exception class, that indicates that a unary operator overload was not found.
    
    Attributes:
        token (lexer.Symto): The unary operator token.
        typename (objects.Typename): The parameter typename.
    """

    def __init__(self, token, typename):
        """
        Initialize the object.

        Args:
            token (lexer.Symto): The unary operator token.
            typename (objects.Typename): The typename of the operator parameter.
        """
        super().__init__(token.anchor)
        self.token = token
        self.typename = typename

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'Could not find any matching unary operator overloads for "{0}({1})".'.format(str(self.token), str(self.typename))

class FunctionOverloadNotFoundError(SourceError):
    """
    An exception class, that indicates that a function overload was not found.
    
    Attributes:
        token (lexer.Symto): The function token.
        parameters ([objects.Parameters]): The parameters of the function.
    """

    def __init__(self, token, parameters):
        """
        Initialize the object.

        Args:
            token (lexer.Symto): The function token.
            parameters ([objects.Parameters]): The parameters of the function.
        """
        super().__init__(token.anchor)
        self.token = token
        self.parameters = parameters

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'Could not find any matching function overloads for "{0}({1})".'.format(str(self.token), Algorithm.join_comma(self.parameters))

class InvalidArrayIndexTypeError(SourceError):
    """An exception class, that indicates that an array index is not a valid type."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + 'Invalid array index type. Array indices must be integers.'

class InvalidArrayDimensionsError(SourceError):
    """An exception class, that indicates that an invalid array bounds was provided."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Invalid array dimensions."

class InvalidArrayIndexDimensionsError(SourceError):
    """
    An exception class, that indicates that an invalid array index was provided.
    
    Attributes:
        expectedDims (list): The expected array dimensions.
    """

    def __init__(self, anchor, expectedDims):
        """
        Initialize the object.

        Args:
            anchor (lexer.Anchor): The anchor.
            expectedDims (list): The expected array dimensions.
        """
        super().__init__(anchor)
        self.expectedDims = expectedDims

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Invalid array index dimensions. Expected {0}-tuple.".format(str(self.expectedDims))

class NativeTypenameArrayError(Exception):
    """
    An exception class, that indicates that a native typename was specified as an array.
    
    Attributes:
        typename (str): The native typename string.
    """

    def __init__(self, typename):
        """
        Initialize the object.

        Args:
            typename (str): The native typename string.
        """
        self.typename = typename

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return "Native typename {0} must not be an array.".format(self.typename)

class PredicateExpectedError(SourceError):
    """An exception class, that indicates that a predicate was expected but not provided."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Predicate expected."

class ReturnTypeMismatchError(SourceError):
    """An exception class, that indicates that the return type does not match the expression type."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Return type mismatch."

class NotInsideLoopError(SourceError):
    """An exception class, that indicates that an instruction was expected to be executed in a loop."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Not inside a loop."

class InvalidElseError(SourceError):
    """An exception class, that indicates that an else-branch was used incorrectly."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Default branches have to follow conditional branches."

class InvalidElifError(SourceError):
    """An exception class, that indicates that an elif-branch was used incorrectly."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Conditional branch has to precede another conditional branch."

class VariableTypeMismatchError(SourceError):
    """An exception class, that indicates that a variable was already initialized with a different type."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "The variable has already been initialized with a different type."

class SetNotSupportedError(SourceError):
    """An exception class, that indicates that a set operation is not supported."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "The property does not support write access."

class InvalidAliasDimensionsError(SourceError):
    """An exception class, that indicates that aliases must be scalar."""

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return super().__str__() + "Aliases with underlying array types may only be referenced as scalars."

class CircularDependencyError(Exception):
    """
    An exception class, that indicates a circular dependency.

    Attributes:
        dependencyChain (generator): The dependency chain.
    """

    def __init__(self, dependencyChain):
        """
        Initialize the object.

        Args:
            dependencyChain (generator): The dependency chain.
        """
        super().__init__()
        self.dependencyChain = dependencyChain

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return "Circular dependency detected: [{0}].".format(Algorithm.join(" -> ", self.dependencyChain))
