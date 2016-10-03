import io
from enum import Enum
from pygments.token import Token, Text, Operator, Name, String, Number, Punctuation, Error
from enum import Enum
from copy import deepcopy
from functools import reduce
from symbolic.exceptions import *
from symbolic.lexer import SymbolicLexer, Symto
from symbolic.formatter import PrettyString
from symbolic.algorithm import Algorithm
from builtins import AssertionError

class Annotateable:
    def __init__(self, userAnnotations, sysAnnotations, semantic):
        self.userAnnotations = userAnnotations
        self.sysAnnotations = sysAnnotations
        self.semantic = semantic

class Named(Annotateable):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token):
        super().__init__(userAnnotations, sysAnnotations, semantic)
        self.token = token

class TemplateObject(Named):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token, body):
        super().__init__(userAnnotations, sysAnnotations, semantic, token)
        self.body = body

class Variable:
    def __init__(self, typename):
        self.typename = typename

class InstructionKind(Enum):
    Expression = 0
    # Jmps
    Break = 1
    Return = 2
    Continue = 3
    # If has to be first here
    If = 4
    For = 5
    While = 6
    Do = 7
    Elif = 8
    Else = 9

class Instruction(Annotateable):
    def __init__(self, userAnnotations, sysAnnotations, semantic, kind, expression=None, instructions=None, forInit=None, forWhile=None, forStep=None):
        super().__init__(userAnnotations, sysAnnotations, semantic)
        self.kind = kind
        self.expression = expression
        self.instructions = instructions
        self.forInit = forInit
        self.forWhile = forWhile
        self.forStep = forStep

    @staticmethod
    def expect_expression(parser, endDelim):
        expression = Expression.parse(parser, [endDelim])
        if (expression is None) or (not expression.postfixTokens):
            raise MissingExpressionError(parser.token)
        parser.expect(endDelim)
        return expression

    @staticmethod
    def parse_parenthesized_expression(parser):
        parser.expect('(')
        expression = Instruction.expect_expression(parser, ')')
        semantic = Annotation.parse_semantic(parser)
        return expression, semantic

    @staticmethod
    def parse_instruction_body(parser):
        parser.expect('{')
        instructions = parser.gather_objects([Instruction], args=['}'])
        parser.expect('}')
        return instructions

    @staticmethod
    def parse(parser, args):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)
        
        # Control flow statements
        ikMap = [(e.name.lower(), e) for e in InstructionKind][InstructionKind.If.value:] # Ignore Expression
        kind = parser.match_map(InstructionKind.Expression, ikMap)

        if kind != InstructionKind.Expression:
            if kind not in [InstructionKind.Do, InstructionKind.For, InstructionKind.Else]:
                # KEYWORD(EXPRESSION)
                expression, semantic = Instruction.parse_parenthesized_expression(parser)
                instructions = Instruction.parse_instruction_body(parser)
                return Instruction(userAnnotations, sysAnnotations, semantic, kind, expression=expression, instructions=instructions)
            elif kind == InstructionKind.Do:
                # DO { ... } WHILE(EXPRESSION)
                instructions = Instruction.parse_instruction_body(parser)
                parser.expect('while')
                expression, semantic = Instruction.parse_parenthesized_expression(parser)
                return Instruction(userAnnotations, sysAnnotations, semantic, kind, expression=expression, instructions=instructions)
            elif kind == InstructionKind.For:
                # FOR(INIT_EXPR, INIT_EXPR; COND, COND; STEP, STEP)
                parser.expect('(')
                forInit = parser.gather_objects([Expression], ',', args=[',', ';'])
                parser.expect(';')
                forWhile = parser.gather_objects([Expression], ',', args=[',', ';'])
                parser.expect(';')
                forStep = parser.gather_objects([Expression], ',', args=[',', ')'])
                parser.expect(')')
                semantic = Annotation.parse_semantic(parser)
                instructions = Instruction.parse_instruction_body(parser)
                return Instruction(userAnnotations, sysAnnotations, semantic, kind, instructions=instructions, forInit=forInit, forWhile=forWhile, forStep=forStep)
            elif kind == InstructionKind.Else:
                # ELSE { ... }
                semantic = Annotation.parse_semantic(parser)
                instructions = Instruction.parse_instruction_body(parser)
                return Instruction(userAnnotations, sysAnnotations, semantic, kind, instructions=instructions)
            else:
                raise DevError()
        else:
            # Early-out if the current token is one of the end delimiters
            if parser.token.text in args:
                return None

            ikMap = [(e.name.lower(), e) for e in InstructionKind][InstructionKind.Break.value:InstructionKind.If.value]
            kind = parser.match_map(InstructionKind.Expression, ikMap)

            if kind in [InstructionKind.Expression, InstructionKind.Return]:
                expression = parser.try_parse_any([Expression], [';', '{']) # Disambiguation with nested functions
                if expression is None:
                    return None

            if parser.match('{'): # Disambiguation with nested functions
                return None

            parser.expect(';')

            # Forward annotations
            expression.userAnnotations = userAnnotations
            expression.sysAnnotations = sysAnnotations

            return Instruction(None, None, None, kind, expression=expression) 

class ExpressionAtomKind(Enum):
    Var = 0
    Number = 1
    FunctionBegin = 2
    FunctionEnd = 3
    ArrayBegin = 4
    ArrayEnd = 5
    TemplateBegin = 6
    TemplateEnd = 7
    UnaryOp = 8
    BinaryOp = 9
    Delimiter = 10

class ExpressionAtom:
    def __init__(self, token, kind):
        self.token = token
        self.kind = kind

class ExpressionAST:
    def __init__(self, atom, parent, children=[]):
        self.atom = atom
        self.parent = parent
        self.children = children

class Expression(Annotateable):
    def __init__(self, userAnnotations, sysAnnotations, tokens):
        super().__init__(userAnnotations, sysAnnotations, None)
        self.tokens = tokens
        self.postfixAtoms = Expression.to_postfix(tokens)
        self.tree = Expression.to_ast(self.postfixAtoms)

    @staticmethod
    def to_postfix(tokens):
        class State(Enum):
            Default = 0
            Function = 1
            Array = 2
            Template = 3
            Tuple = 4

        out, stack, states = [], [], [State.Default]
        isNextOpenParenFunction = False
        wasLastTerminal = False
        # K=2 lookahead
        for i, t in enumerate(tokens):
            prev = tokens[i-1] if i > 0 else None
            t1 = tokens[i+1] if i < len(tokens) - 1 else None
            t2 = tokens[i+2] if i < len(tokens) - 2 else None

            if t.isTerminal:
                if wasLastTerminal:
                    raise InvalidExpressionError(t)

                # Set state for next cycle
                wasLastTerminal = True

                kind = ExpressionAtomKind.Number if t.isNumber else ExpressionAtomKind.Var
                # K=1 lookahead
                if t1 is not None:
                    # Function, Template
                    if t.kind == Token.Name:
                        if t1.text == '(':
                            kind = ExpressionAtomKind.FunctionBegin
                            states.append(State.Function)
                            stack.append(ExpressionAtom(t, ExpressionAtomKind.FunctionEnd))
                            isNextOpenParenFunction = True
                        elif t1.text == '<':
                            # K=2 lookahead
                            if (not t2 is None) and (t2.kind == Token.Literal.String):
                                kind = ExpressionAtomKind.TemplateBegin
                                states.append(State.Template)
                                stack.append(ExpressionAtom(t, ExpressionAtomKind.TemplateEnd))

                out.append(ExpressionAtom(t, kind))
            else:
                # We can fetch a literal in the next cycle again
                wasLastTerminal = False

                if t.text == '[':
                    # If this is an Array the last output token has to be a terminal, a function, an array or a template
                    if not out:
                        raise MissingArrayTypeError(t)

                    if not out[-1].kind in [ExpressionAtomKind.Var, ExpressionAtomKind.FunctionBegin, ExpressionAtomKind.FunctionEnd, ExpressionAtomKind.ArrayBegin, ExpressionAtomKind.ArrayEnd, ExpressionAtomKind.TemplateBegin, ExpressionAtomKind.TemplateEnd]:
                        raise InvalidArrayTypeError(out[-1].token)

                    if (not prev.text in SymbolicLexer.closeBrackets) and (prev.kind != Token.Name):
                        raise InvalidArrayTypeError(out[-1].token)

                    out.append(ExpressionAtom(t, ExpressionAtomKind.ArrayBegin))
                    states.append(State.Array)
                    stack.append(ExpressionAtom(t, ExpressionAtomKind.ArrayEnd))
                elif t.text == ',':
                    # Tuples > 1 are not allowed
                    if states[-1] == State.Tuple:
                        raise InvalidExpressionError(t)

                    # Keep track of how many ops were added
                    if Algorithm.pop_while(stack, lambda atom: not atom.token.isOpenBracket, lambda atom: out.append(atom)):
                        raise MissingBracketsError(t)
                
                    # If there were no ops added, the comma might be invalid if this is not an unbounded array
                    if states[-1] != State.Array:
                        if out[-1].kind in [ExpressionAtomKind.Delimiter, ExpressionAtomKind.FunctionBegin, ExpressionAtomKind.TemplateBegin]:
                            raise InvalidExpressionError(t)

                    # Add comma as delimiter
                    out.append(ExpressionAtom(t, ExpressionAtomKind.Delimiter))
                elif (t.isOpenBracket and t.text != '<') or (t.text == '<' and states[-1] == State.Template):
                    if t.text == '(':
                        # Is this a potential tuple?
                        if not isNextOpenParenFunction:
                            states.append(State.Tuple)
                        else:
                            isNextOpenParenFunction = False
                    stack.append(ExpressionAtom(t, -1))
                elif (t.isCloseBracket and t.text != '>') or (t.text == '>' and states[-1] == State.Template): # Special case for template >
                    # Keep track of how many parameters were added
                    if Algorithm.pop_while(stack, lambda atom: not atom.token.text == t.matchingOpenBracket, lambda atom: out.append(atom)):
                        raise MissingBracketsError(t)

                    # Put array symbol on output stream
                    if t.text == ']':
                        out.append(ExpressionAtom(t, ExpressionAtomKind.ArrayEnd))

                    # Pop open bracket and state
                    stack.pop()
                    states.pop()

                    if stack:
                        # Pop function, template
                        if stack[-1].kind in [ExpressionAtomKind.FunctionEnd, ExpressionAtomKind.TemplateEnd]:
                            out.append(stack[-1])
                            stack.pop()
                elif t.kind is Operator:
                    # Assume this is a unary op
                    kind = ExpressionAtomKind.UnaryOp if (t.isUnaryOp and prev is None) or (prev is not None and prev.text != ')' and not prev.isTerminal) else ExpressionAtomKind.BinaryOp
                    o1 = t
                
                    # Binary operator
                    if kind == ExpressionAtomKind.BinaryOp:
                        Algorithm.pop_while(stack, lambda o2: (o2.token.isOp) and ((o1.isBinaryLeftAssociative and o1.binaryPrecedence > o2.token.binaryPrecedence) or (o1.isBinaryRightAssociative and o1.binaryPrecedence >= o2.token.binaryPrecedence)), lambda o2: out.append(o2))
                
                    stack.append(ExpressionAtom(o1, kind))
                else:
                    raise InvalidExpressionError(t)

        # Remaining tokens to output
        if not Algorithm.pop_while(stack, lambda atom: not atom.token.text == '(', lambda atom: out.append(atom)):
            raise MissingBracketsError(stack[-1].token)

        return out

    @staticmethod
    def to_ast(postfixAtoms):
        argCount = 0
        argCountStack = []
        argStack = []
        parent = None

        for atom in postfixAtoms:
            root = ExpressionAST(atom, parent)

            if atom.kind in [ExpressionAtomKind.Var, ExpressionAtomKind.Number]:
                argCount += 1 if argCount == 0 else 0
                argStack.append(root)
            elif atom.kind in [ExpressionAtomKind.FunctionBegin, ExpressionAtomKind.ArrayBegin, ExpressionAtomKind.TemplateBegin]:
                argCount += 1 if argCount == 0 else 0
                argCountStack.append(argCount)
                argCount = 0
                argStack.append(root)
                parent = root
            elif atom.kind in [ExpressionAtomKind.FunctionEnd, ExpressionAtomKind.ArrayEnd, ExpressionAtomKind.TemplateEnd]:
                numArgs = argCount+1
                if len(argStack) < numArgs:
                    raise InvalidExpressionError(atom.token)

                args = list(argStack[-numArgs:])

                # Replace root (End) with Begin
                root = args[0] # Begin

                args = args[1:] # Pop Begin
                argStack = argStack[:-numArgs]
                root.children = args
                argStack.append(root)
                parent = root.parent

                # Restore argument count
                argCount = argCountStack[-1]
                argCountStack.pop()
            elif atom.kind == ExpressionAtomKind.UnaryOp:
                if len(argStack) < 1:
                    raise InvalidExpressionError(atom.token)

                argCount += 1 if argCount == 0 else 0
                child = argStack[-1]
                argStack = argStack[:-1]
                root.children = [child]
                argStack.append(root)
                parent = root.parent
            elif atom.kind == ExpressionAtomKind.BinaryOp:
                if len(argStack) < 2:
                    raise InvalidExpressionError(atom.token)

                argCount += 1 if argCount == 0 else 0
                lhs = argStack[-2]
                rhs = argStack[-1]
                argStack = argStack[:-2]
                root.children = [lhs, rhs]
                argStack.append(root)
                parent = root.parent
            elif atom.kind == ExpressionAtomKind.Delimiter:
                argCount += 1 if argCount > 0 else 2
            else:
                raise DevError()

        # Empty expression
        if not argStack:
            return None

        # Invalid expression
        if len(argStack) > 1:
            raise InvalidExpressionError(argStack[0].atom.token)

        # Just right! :)
        return argStack[0]

    @staticmethod
    def parse(parser, args):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)
        tokens = parser.until_any(args)
        try:
            return Expression(userAnnotations, sysAnnotations, tokens)
        except:
            return None

class Namespace(Named):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token):
        super().__init__(userAnnotations, sysAnnotations, semantic, token)
        self.objects = []

        if Annotation.is_not_compatible(['private', 'deprecate'], sysAnnotations):
            raise UnsupportedSystemAnnotationsError(self.token, 'Namespace', sysAnnotations)

    @staticmethod
    def parse(parser, args):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        if not parser.match('namespace'):
            return None

        token = parser.expect_kind(Token.Name)
        semantic = Annotation.parse_semantic(parser)

        parser.expect('{')
        
        # Push the namespace onto the scope stack
        namespace = Namespace(userAnnotations, sysAnnotations, semantic, token)
        parser.namespaceStack[-1].objects.append(namespace)
        parser.namespaceStack.append(namespace)

        # Parse all objects inside it
        parser.gather_namespace_objects()

        # Exit the namespace
        parser.expect('}')
        parser.namespaceStack.pop()

        return namespace

    @staticmethod
    def parse_explicit_scoping(parser):
        token = parser.match_kind(Token.Name)
        if token is None:
            return []

        scope = [token]
        while True:
            token = parser.token
            if parser.match('.'):
                scope += [parser.expect_kind(Token.Name)]
            else:
                break
        return scope

class FunctionKind(Enum):
    Regular = 0
    Operator = 1
    Extension = 2 

class FunctionSignature:
    def __init__(self, kind, inArgs, outArgs):
        self.kind = kind
        self.inArgs = inArgs
        self.outArgs = outArgs

class Parameter(Named):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token, typename, isRef):
        super().__init__(userAnnotations, sysAnnotations, semantic, token)
        self.typename = typename
        self.isRef = isRef

    @staticmethod
    def parse(parser, args):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        isRef = parser.match('ref')

        typename = Typename.try_parse(parser)
        if typename is None:
            return None

        name = parser.match_kind(Token.Name)
        semantic = Annotation.parse_semantic(parser)

        # Handle unnamed parameter
        if name is None:
            name = deepcopy(typename.scope[-1])
            name.column = name.columnEnd + 1
            name.text = ''

        return Parameter(userAnnotations, sysAnnotations, semantic, name, typename, isRef)

class Function(TemplateObject):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token, body, kind, returnTypename, extensionTargetName, parameters):
        super().__init__(userAnnotations, sysAnnotations, semantic, token, body)
        self.kind = kind
        self.returnTypename = returnTypename
        self.extensionTargetName = extensionTargetName
        self.extensionName = '.'.join([t.text for t in extensionTargetName] + [token.text]) if extensionTargetName is not None else None
        self.parameters = parameters
        self.name = token.text

        if kind == FunctionKind.Extension:
            fail = Annotation.is_not_compatible(['static', 'private', 'deprecate'], sysAnnotations)
        elif kind == FunctionKind.Operator:
            fail = Annotation.is_not_compatible(['private', 'deprecate'], sysAnnotations)
        elif kind == FunctionKind.Regular:
            fail = Annotation.is_not_compatible(['private', 'deprecate'], sysAnnotations)
        if fail:
            raise UnsupportedSystemAnnotationsError(self.token, 'Function', sysAnnotations)

    @staticmethod
    def parse_template(parser, isTemplate):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        kind = FunctionKind.Regular
        name = Symto(parser.token.kind, parser.token.fileName, '', parser.token.line, parser.token.column)
        extensionTargetName = None
        parameters = []
        returnTypename = Typename.try_parse(parser)
        hasExplicitReturnType = returnTypename is not None

        # If the next token is not an ID then returnTypename
        # is actually the function name / scope and the return type is implicit void
        if parser.token.kind != Token.Name:
            if hasExplicitReturnType:
                name = returnTypename.scope[-1]
            returnTypenameToken = deepcopy(parser.token)
            returnTypenameToken.text = 'void'
            returnTypename = Typename([returnTypenameToken], [], [])
        
        if hasExplicitReturnType:
            # Extensions can be explicitly scoped
            scope = Namespace.parse_explicit_scoping(parser)
            if len(scope) >= 2:
                # Looks like an extension
                kind = FunctionKind.Extension
                name = scope[-1]
                extensionTargetName = scope[:-1]
            else:
                if len(scope) == 1:
                    # Regular function or operator
                    name = scope[0]

                # Operators require 1 extra token
                if name.text == 'operator':
                    kind = FunctionKind.Operator
                    name = parser.expect_kind(Token.Operator)

            if parser.match('('):
                parameters = parser.gather_objects([Parameter], ',')
                parser.expect(')')

        semantic = Annotation.parse_semantic(parser)

        body = None
        if isTemplate:
            body = parser.fetch_block('{', '}')
            parser.match(';')
        else:
            if parser.match('{'):
                objects = parser.gather_objects([Struct, Alias, Template, Instruction, Function], args=['}'])
                parser.expect('}')
                parser.match(';')
            elif not parser.match(';'):
                return None
        
        # Register the function with the current namespace
        func = Function(userAnnotations, sysAnnotations, semantic, name, body, kind, returnTypename, extensionTargetName, parameters)
        parser.namespaceStack[-1].objects.append(func)

        return func

    @staticmethod
    def parse(parser, args):
        return Function.parse_template(parser, False)

    def generate_from_template(self, prettyString):
        # ReturnType
        prettyString += str(self.returnTypename)

        # Name
        if self.kind == FunctionKind.Extension:
            prettyString += self.extensionName
        elif self.kind == FunctionKind.Operator:
            prettyString += 'operator' + self.name
        else:
            prettyString += self.name

        # Parameter signature
        numParameters = len(self.parameters)
        if numParameters > 0:
            prettyString += '('
            prettyString += '\n'
            prettyString.indentLevel += 1
            if numParameters > 0:
                for i in range(0, numParameters):
                    p = self.parameters[i]
                    prettyString += Annotation.usrlist_to_str(p.userAnnotations)
                    prettyString += Annotation.syslist_to_str(p.sysAnnotations)
                    if p.isRef:
                        prettyString += 'ref'
                    prettyString += p.typename.scopeString
                    prettyString += p.token.text
                    if p.semantic is not None:
                        prettyString += ': ' + p.semantic.text
                    if i < numParameters - 1:
                        prettyString += ','
                    prettyString += '\n'
            prettyString.indentLevel -= 1
            prettyString += ')'

class Member(Named):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token, typename):
        super().__init__(userAnnotations, sysAnnotations, semantic, token)
        self.typename = typename

        if Annotation.is_not_compatible(['private', 'deprecate'], sysAnnotations):
            raise UnsupportedSystemAnnotationsError(self.token, 'Member', sysAnnotations)

    @staticmethod
    def parse(parser, args):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)
        typename = Typename.try_parse()
        if typename is None:
            return None

        name = parser.expect_kind(Token.Name)
        semantic = Annotation.parse_semantic(parser)
        
        parser.match(';')

        return Member(userAnnotations, sysAnnotations, semantic, name, typename)

class MemberList:
    @staticmethod
    def parse(parser, args):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)
        typename = Typename.try_parse(parser)
        if typename is None:
            return None

        names = [parser.expect_kind(Token.Name)]
        while parser.match(','):
            name = parser.match_kind(Token.Name)
            if name is None:
                break
            names.append(name)

        semantic = Annotation.parse_semantic(parser)

        parser.match(';')
        
        result = []
        for name in names:
            result.append(Member(userAnnotations, sysAnnotations, semantic, name, typename))
        
        return result

class Struct(TemplateObject):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token, members, body):
        super().__init__(userAnnotations, sysAnnotations, semantic, token, body)
        self.members = members

        if Annotation.is_not_compatible(['private', 'deprecate'], sysAnnotations):
            raise UnsupportedSystemAnnotationsError(self.token, 'Struct', sysAnnotations)

    @staticmethod
    def parse_template(parser, isTemplate):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        if not parser.match('struct'):
            return None

        token = parser.expect_kind(Token.Name)
        semantic = Annotation.parse_semantic(parser)

        members = []
        body = None
        if isTemplate:
            body = parser.fetch_block('{', '}')
        else:
            parser.expect('{')
            memberLists = parser.gather_objects([MemberList])
            parser.expect('}')
            # Fold member lists
            members = reduce((lambda l, r: l + r), memberLists) if memberLists else []
        parser.match(';')

        # Register the struct with the current namespace
        struct = Struct(userAnnotations, sysAnnotations, semantic, token, members, body)
        parser.namespaceStack[-1].objects.append(struct)

        return struct

    @staticmethod
    def parse(parser, args):
        return Struct.parse_template(parser, False)

    def generate_from_template(self, prettyString):
        prettyString += 'struct ' + self.token.text

class Alias(TemplateObject):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token, body, targetType):
        super().__init__(userAnnotations, sysAnnotations, semantic, token, body)
        self.targetType = targetType

        if Annotation.is_not_compatible(['private', 'deprecate'], sysAnnotations):
            raise UnsupportedSystemAnnotationsError(self.token, 'Alias', sysAnnotations)

    @staticmethod
    def parse_template(parser, isTemplate):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)
        if not parser.match('using'):
            return None

        name = parser.expect_kind(Token.Name)
        semantic = Annotation.parse_semantic(parser)
        targetType = None
        body = None
        if isTemplate:
            body = parser.fetch_block('{', '}')
        else:
            parser.expect('{')
            targetType = Typename.parse(parser)
            parser.expect('}')
        parser.match(';')

        # Register the struct with the current namespace
        alias = Alias(userAnnotations, sysAnnotations, semantic, name, body, targetType)
        parser.namespaceStack[-1].objects.append(alias)

        return alias

    @staticmethod
    def parse(parser, args):
        return Alias.parse_template(parser, False)

    def generate_from_template(self, prettyString):
        prettyString += 'using ' + self.token.text

class Reference(Annotateable):
    def __init__(self, userAnnotations, sysAnnotations, semantic, string):
        super().__init__(userAnnotations, sysAnnotations, semantic)
        self.string = string

        if sysAnnotations:
            raise UnsupportedSystemAnnotationsError(self.token, 'Reference', sysAnnotations)

    def __str__(self):
        return self.string

class Annotation:
    def __init__(self, token, args):
        super().__init__()
        self.token = token
        self.args = args

    @staticmethod
    def list_to_str(collection, open, close = None):
        result = ''
        for annotation in collection:
            result += open + annotation.token.text
            if len(annotation.args) > 0:
                result += '(' + ', '.join([t.text for t in annotation.args]) + ')'
            if close is not None:
                result += close
            result += '\n'
        return result

    @staticmethod
    def usrlist_to_str(collection):
        return Annotation.list_to_str(collection, '[', ']')

    @staticmethod
    def syslist_to_str(collection):
        return Annotation.list_to_str(collection, '@')

    @staticmethod
    def parse_annotation_interior(parser):
        token = parser.expect_kind(Token.Name)
        parameters = parser.fetch_block('(', ')')
        parameters = [p for p in parameters if p.text != ',']
        return Annotation(token, parameters)

    @staticmethod
    def try_parse_sys(parser):
        if parser.match('@'):
            annotation = Annotation.parse_annotation_interior(parser)
            return annotation
        else:
            return None

    @staticmethod
    def try_parse_user(parser):
        if parser.match('['):
            annotation = Annotation.parse_annotation_interior(parser)
            parser.match(']')
            return annotation
        else:
            return None

    @staticmethod
    def parse_semantic(parser):
        return Annotation.parse_annotation_interior(parser) if parser.match(':') else None

    @staticmethod
    def parse_annotations(parser):
        userAnnotations = []
        sysAnnotations = []
        while True:
            annotation = Annotation.try_parse_user(parser)
            if annotation is None:
                annotation = Annotation.try_parse_sys(parser)
                if annotation is None: break

                # Make sure the system annotation exists
                if not annotation.token.text in Annotation.sys_annotations():
                    raise UnknownSystemAnnotationError(annotation.token, annotation.token.text)

                sysAnnotations.append(annotation)
            else:
                userAnnotations.append(annotation)
        return userAnnotations, sysAnnotations

    @staticmethod
    def sys_annotations():
        return ['static', 'private', 'noconstructor', 'deprecate']

    @staticmethod
    def has(name, collection):
        return any(e.token.text == name for e in collection)
        
    @staticmethod
    def is_not_compatible(compatibleNames, collection):
        return any(not e.token.text in compatibleNames for e in collection)

    @staticmethod
    def verify_annotation(annotations, name, signatures):
        for annotation in annotations:
            if annotation.token.text == name:
                isSigValid = False
                for sig in signatures:
                    # Signature length has to match
                    if (len(annotation.args) == len(sig)):
                        # Signature Token types have to match
                        isSigMatch = True
                        for i in range(0, len(sig)):
                            if annotation.args[i].kind != sig[i]:
                                isSigMatch = False
                                break
                        if isSigMatch:
                            isSigValid = True
                            break
                if not isSigValid:
                    raise MissingAnnotationArgsError(annotation.token, signatures)

class Typename:
    def __init__(self, scope, templateArgs, dims):
        super().__init__()
        self.scope = scope
        self.templateArgs = templateArgs
        self.dims = dims
        self.scopeString = '.'.join([tok.text for tok in scope])

    @staticmethod
    def try_parse(parser):
        # Typenames can be explicitly scoped
        scope = Namespace.parse_explicit_scoping(parser)
        if not scope:
            return None

        # Template args
        templateArgs = []
        if parser.match('<'):
            while True:
                token = parser.match_kind(Token.Literal.String)
                if token is None:
                    break
                templateArgs += [token]
                parser.match(',')
            parser.expect('>')

        # Array
        dims = []
        if parser.match('['):
            missingBounds = True
            while True:
                # Known bounds
                token = parser.match_kind(Token.Number.Integer)
                if token is not None:
                    dims += [token]
                    missingBounds = False
                else:
                    # Unknown bounds
                    if missingBounds:
                        dims += [None]
                        missingBounds = False

                    token = parser.token
                    if parser.match(','):
                        missingBounds = True
                    else:
                        break
            parser.expect(']')

        return Typename(scope, templateArgs, dims)

    @staticmethod
    def parse(parser):
        typename = Typename.try_parse(parser)
        if typename is None:
            raise TypenameExpectedError(parser.token)
        return typename

    def __str__(self):
        return self.scopeString

class TemplateParameter(Named):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token):
        super().__init__(userAnnotations, sysAnnotations, semantic, token)

    @staticmethod
    def parse(parser, args):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        name = parser.match_kind(Token.Name)
        if name is None:
            return None

        semantic = Annotation.parse_semantic(parser)

        return TemplateParameter(userAnnotations, sysAnnotations, semantic, name)

class Template(Named):
    def __init__(self, userAnnotations, sysAnnotations, semantic, parameters, obj, namespaceList, references):
        super().__init__(userAnnotations, sysAnnotations, semantic, obj.token)
        self.obj = obj
        self.parameters = parameters
        self.namespaceList = namespaceList
        self.namespaceList.pop(0)
        self.references = references

        if Annotation.is_not_compatible(['private', 'deprecate'], sysAnnotations):
            raise UnsupportedSystemAnnotationsError(self.token, 'Template', sysAnnotations)

    def generate_translation_unit(self):
        result = PrettyString()

        # Namespace
        for namespace in self.namespaceList:
            result += Annotation.usrlist_to_str(namespace.userAnnotations)
            result += Annotation.syslist_to_str(namespace.sysAnnotations)
            result += 'namespace {0}'.format(namespace.token.text)
            if namespace.semantic is not None:
                result += ': ' + namespace.semantic.text
            result += ' {\n'
            result.indentLevel += 1

        # Emit the annotations
        result += Annotation.usrlist_to_str(self.userAnnotations)
        result += Annotation.syslist_to_str(self.sysAnnotations)

        # Emit object header
        self.obj.generate_from_template(result)

        # Template body
        if self.obj.semantic is not None:
            result += ': ' + self.obj.semantic.text
        result += ' {'
        result += '\n'
        result.indentLevel += 1
        result.value += PrettyString.from_tokens(self.obj.body, self.obj.body[0].line) if len(self.obj.body) > 0 else 1
        result += '\n'
        result.indentLevel -= 1
        result += '}'
        result += '\n'

        # Close namespace
        for namespace in self.namespaceList:
            result.indentLevel -= 1
            result += "}\n"

        return result.value.strip()

    @staticmethod
    def parse(parser, args):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        if not parser.match('template'):
            return None

        parameters = []
        if parser.match('<'):
            parameters = parser.gather_objects([TemplateParameter], ',')
            parser.expect('>')
        semantic = Annotation.parse_semantic(parser)

        # Deduce the template kind
        templateClasses = [Struct, Alias, Function]
        for c in templateClasses:
            parser.push_state()
            obj = c.parse_template(parser, True)
            if obj is not None:
                parser.remove_state()
                break
            parser.pop_state()

        if obj is None:
            return None

        # Register the template with the current namespace
        template = Template(userAnnotations, sysAnnotations, semantic, parameters, obj, list(parser.namespaceStack), list(parser.references))
        parser.namespaceStack[-1].objects.append(template)

        return template