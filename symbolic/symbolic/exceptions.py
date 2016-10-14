class TokenError(Exception):
    '''An exception base class, that indicates an error that can be located in the source.'''
    def __init__(self, token):
        '''
        Initializes the object.

        Args:
            token (Symto): The token to associate with this error.
        '''
        super().__init__()
        self.token = token
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return "'{0}' @ ({1}, {2}): ".format(self.token.fileName, self.token.line, self.token.column)

class DevError(Exception):
    '''An exception class, that indicates a developer error (bug).'''
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return TokenError.__str__(self) + "Developer error."

class UnexpectedTokenError(TokenError):
    '''An exception class, that indicates a mismatch between an expected token and the current token in the token stream.'''
    def __init__(self, token, expected, found):
        '''
        Initializes the object.

        Args:
            token (Symto): The token to associate with this error.
            expected (Symto): The expected token.
            found (Symto): The current token in the token stream.
        '''
        super().__init__(token)
        self.expected = expected
        self.found = found
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "Found '{0}' but expected '{1}'.".format(str(self.found), str(self.expected))

class UnexpectedEOFError(Exception):
    '''An exception class, that indicates an unexpected EOF token in the token stream.'''
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "Unexpected EOF."

class UnsupportedSystemAnnotationError(TokenError):
    '''An exception class, that indicates an unsupported system annotation.'''
    def __init__(self, what, sysAnnotation):
        '''
        Initializes the object.

        Args:
            token (Symto): The token to associate with this error.
            what (Symto): The expected token.
            sysAnnotation (list(Annotation)): The system annotation.
        '''
        super().__init__(sysAnnotation.token)
        self.what = what
        self.sysAnnotation = sysAnnotation
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "{0}s do not support the '{1}' system annotation.".format(self.what, str(self.sysAnnotation.token))

class UnknownSystemAnnotationError(TokenError):
    '''An exception class, that indicates an unknown system annotation.'''
    def __init__(self, token, sysAnnotation):
        '''
        Initializes the object.

        Args:
            token (Symto): The token to associate with this error.
            sysAnnotation (Annotation): The system annotation.
        '''
        super().__init__(token)
        self.sysAnnotation = sysAnnotation
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "{0} is not a known system annotation.".format(str(self.sysAnnotation))

class MissingScopeError(TokenError):
    '''An exception class, that indicates a missing scope.'''
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "Missing scope."

class TypenameExpectedError(TokenError):
    '''An exception class, that indicates a missing typename.'''
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "Typename expected."

class MissingExpressionError(TokenError):
    '''An exception class, that indicates a missing expression.'''
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "Missing expression."

class MissingBracketsError(TokenError):
    '''An exception class, that indicates missing brackets.'''
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "Missing brackets."

class InvalidExpressionError(TokenError):
    '''An exception class, that indicates an invalid expression.'''
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "Invalid expression."

class MissingArrayTypeError(TokenError):
    '''An exception class, that indicates a missing array accessor type.'''
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "Missing array accessor type."

class ExpectedEOFError(TokenError):
    '''An exception class, that indicates an expected EOF.'''
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "Expected EOF."

class DuplicateTemplateParameterError(TokenError):
    '''An exception class, that indicates duplicate template parameters.'''
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "Duplicate template parameter detected."

class InvalidTypenameError(TokenError):
    '''An exception class, that indicates an invalid typename.'''
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "Invalid typename."

class InvalidNameError(TokenError):
    '''An exception class, that indicates an invalid name.'''
    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return super().__str__() + "Invalid name."