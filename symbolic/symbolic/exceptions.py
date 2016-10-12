class SymbolicError(Exception):
    def __init__(self, token):
        Exception.__init__(self)
        self.token = token
    def __str__(self):
        return "{0} @ ({1}, {2}): ".format(repr(self.token.fileName), self.token.line, self.token.column)

class DevError(Exception):
    def __str__(self):
        return SymbolicError.__str__(self) + "Developer error."

class UnexpectedTokenError(SymbolicError):
    def __init__(self, token, expected, found):
        SymbolicError.__init__(self, token)
        self.expected = expected
        self.found = found
    def __str__(self):
        return SymbolicError.__str__(self) + "Found {0} but expected {1}.".format(repr(self.found), repr(self.expected))

class UnexpectedEOFError(Exception):
    def __str__(self):
        return SymbolicError.__str__(self) + "Unexpected EOF."

class UnsupportedSystemAnnotationsError(SymbolicError):
    def __init__(self, token, what, sysAnnotations):
        SymbolicError.__init__(self, token)
        self.what = what
        self.sysAnnotations = sysAnnotations
    def __str__(self):
        if len(self.sysAnnotations) > 1:
            return SymbolicError.__str__(self) + "{0}s do not support {1} system annotations.".format(self.what, repr([e.token.text for e in self.sysAnnotations]))
        else:
            return SymbolicError.__str__(self) + "{0}s do not support the {1} system annotation.".format(self.what, repr(self.sysAnnotations[0].token.text))

class UnknownSystemAnnotationError(SymbolicError):
    def __init__(self, token, sysAnnotation):
        SymbolicError.__init__(SymbolicError, token)
        self.sysAnnotation = sysAnnotation
    def __str__(self):
        return SymbolicError.__str__(self) + "{0} is not a known system annotation.".format(repr(self.sysAnnotation))

class MissingScopeError(SymbolicError):
    def __str__(self):
        return SymbolicError.__str__(self) + "Missing scope."

class TypenameExpectedError(SymbolicError):
    def __str__(self):
        return SymbolicError.__str__(self) + "Typename expected."

class MissingAnnotationArgsError(SymbolicError):
    def __init__(self, token, *args):
        SymbolicError.__init__(self, token)
        self.args = args
    def __str__(self):
        if len(self.args) == 1:
            return SymbolicError.__str__(self) + "Missing parameters for system annotation {0}. Possible overloads: {1}.".format(repr(self.token.text), self.args)
        else:
            return SymbolicError.__str__(self) + "Missing parameters for system annotation {0}. Possible overloads: {1}.".format(repr(self.token.text), self.args)

class MissingReturnTypeError(SymbolicError):
    def __str__(self):
        return SymbolicError.__str__(self) + "Missing return type."

class MissingExpressionError(SymbolicError):
    def __str__(self):
        return SymbolicError.__str__(self) + "Missing expression."

class MissingBracketsError(SymbolicError):
    def __str__(self):
        return SymbolicError.__str__(self) + "Missing brackets."

class InvalidExpressionError(SymbolicError):
    def __str__(self):
        return SymbolicError.__str__(self) + "Invalid expression."

class MissingArrayTypeError(SymbolicError):
    def __str__(self):
        return SymbolicError.__str__(self) + "Missing array accessor type."

class InvalidArrayTypeError(SymbolicError):
    def __str__(self):
        return SymbolicError.__str__(self) + "Invalid array accessor type."

class ExpectedEOFError(SymbolicError):
    def __str__(self):
        return SymbolicError.__str__(self) + "Expected EOF."

class DuplicateTemplateParameterError(SymbolicError):
    def __str__(self):
        return SymbolicError.__str__(self) + "Duplicate template parameter name detected."

class InvalidTypenameError(SymbolicError):
    def __str__(self):
        return SymbolicError.__str__(self) + "Invalid typename."

class InvalidNameError(SymbolicError):
    def __str__(self):
        return SymbolicError.__str__(self) + "Invalid name."