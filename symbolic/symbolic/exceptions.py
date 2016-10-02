class SymbolicError(Exception):
    def __init__(self, token):
        super().__init__()
        self.token = token
    def __str__(self):
        return "{0} @ ({1}, {2}): ".format(repr(self.token.fileName), self.token.line, self.token.column)

class DevError(Exception):
    def __str__(self):
        return super().__str__() + "Developer error."

class UnexpectedTokenError(SymbolicError):
    def __init__(self, token, expected, found):
        super().__init__(token)
        self.expected = expected
        self.found = found
    def __str__(self):
        return super().__str__() + "Found {0} but expected {1}.".format(repr(self.found), repr(self.expected))

class UnexpectedEOFError(Exception):
    def __str__(self):
        return super().__str__() + "Unexpected EOF."

class UnsupportedSystemAnnotationsError(SymbolicError):
    def __init__(self, token, what, sysAnnotations):
        super().__init__(token)
        self.what = what
        self.sysAnnotations = sysAnnotations
    def __str__(self):
        if len(self.sysAnnotations) > 1:
            return super().__str__() + "{0}s do not support {1} system annotations.".format(self.what, repr([e.token.text for e in self.sysAnnotations]))
        else:
            return super().__str__() + "{0}s do not support the {1} system annotation.".format(self.what, repr(self.sysAnnotations[0].token.text))

class UnknownSystemAnnotationError(SymbolicError):
    def __init__(self, token, sysAnnotation):
        super().__init__(token)
        self.sysAnnotation = sysAnnotation
    def __str__(self):
        return super().__str__() + "{0} is not a known system annotation.".format(repr(self.sysAnnotation))

class MissingScopeError(SymbolicError):
    def __str__(self):
        return super().__str__() + "Missing scope."

class TypenameExpectedError(SymbolicError):
    def __str__(self):
        return super().__str__() + "Typename expected."

class MissingAnnotationArgsError(SymbolicError):
    def __init__(self, token, *args):
        super().__init__(token)
        self.args = args
    def __str__(self):
        if len(self.args) == 1:
            return super().__str__() + "Missing parameters for system annotation {0}. Possible overloads: {1}.".format(repr(self.token.text), self.args)
        else:
            return super().__str__() + "Missing parameters for system annotation {0}. Possible overloads: {1}.".format(repr(self.token.text), self.args)

class MissingReturnTypeError(SymbolicError):
    def __str__(self):
        return super().__str__() + "Missing return type."

class MissingExpressionError(SymbolicError):
    def __str__(self):
        return super().__str__() + "Missing expression."

class MissingBracketsError(SymbolicError):
    def __str__(self):
        return super().__str__() + "Missing brackets."

class InvalidExpressionError(SymbolicError):
    def __str__(self):
        return super().__str__() + "Invalid expression."

class MissingArrayTypeError(SymbolicError):
    def __str__(self):
        return super().__str__() + "Missing array accessor type."

class InvalidArrayTypeError(SymbolicError):
    def __str__(self):
        return super().__str__() + "Invalid array accessor type."