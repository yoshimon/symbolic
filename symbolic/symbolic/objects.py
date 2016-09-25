import io
from pygments.token import Token, Name
from enum import Enum
from copy import deepcopy
from functools import reduce
from symbolic.exceptions import *
from symbolic.lexer import SymbolicLexer, Symto
from symbolic.formatter import PrettyString

class Annotateable:
    def __init__(self, userAnnotations, sysAnnotations):
        self.userAnnotations = userAnnotations
        self.sysAnnotations = sysAnnotations

class Named(Annotateable):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token):
        super().__init__(userAnnotations, sysAnnotations)
        self.semantic = semantic
        self.token = token

class Statement(Annotateable): pass

class Expression(Statement):
    def __init__(self, userAnnotations, sysAnnotations, tokens):
        super().__init__(userAnnotations, sysAnnotations)
        self.tokens = tokens

    @staticmethod
    def try_parse(parser, tokenEndDelims):
        parser.push_annotations()
        tokens = []
        while not any(parser.token.text in delim for delim in endDelims):
            tokens.append(parser.token)
        return Expression(parser.pop_annotations(), tokens)

class Namespace(Named):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token):
        super().__init__(userAnnotations, sysAnnotations, semantic, token)
        self.objects = []

        if Annotation.is_not_compatible(['private', 'deprecate'], sysAnnotations):
            raise UnsupportedSystemAnnotationsError(self.token, 'Namespace', sysAnnotations)

    @staticmethod
    def parse(parser):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        if not parser.match('namespace'):
            return None

        token = parser.expect_kind(Token.Name)
        semantic = parser.expect_kind(Token.Name) if parser.match(':') else None

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
    def parse(parser):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        isRef = parser.match('ref')

        typename = Typename.try_parse(parser)
        if typename is None:
            return None

        name = parser.match_kind(Token.Name)
        semantic = parser.expect_kind(Token.Name) if parser.match(':') else None

        # Handle unnamed parameter
        if name is None:
            name = deepcopy(typename.scope[-1])
            name.column = name.columnEnd + 1
            name.text = ''

        return Parameter(userAnnotations, sysAnnotations, semantic, name, typename, isRef)

class Function(Named):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token, kind, returnTypename, extensionTargetName, parameters, body):
        super().__init__(userAnnotations, sysAnnotations, semantic, token)
        self.kind = kind
        self.returnTypename = returnTypename
        self.extensionTargetName = extensionTargetName
        self.extensionName = '.'.join([t.text for t in extensionTargetName] + [token.text]) if extensionTargetName is not None else None
        self.parameters = parameters
        self.body = body
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

        parser.push_state()
        returnTypename = Typename.try_parse(parser)
        if returnTypename is None:
            parser.pop_state()
            return None

        # If the next token is not an ID then returnTypename
        # is actually the function name / scope and the return type is implicit void
        if parser.token.kind != Token.Name:
            parser.pop_state()
            returnTypenameToken = deepcopy(parser.token)
            returnTypenameToken.text = 'void'
            returnTypename = Typename([returnTypenameToken], [], [])
        else:
            parser.remove_state()

        # Extensions can be explicitly scoped
        extensionTargetName = None
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
            else:
                raise MissingReturnTypeError(returnTypename.token)

            # Operators require 1 extra token
            if name.text == 'operator':
                kind = FunctionKind.Operator
                name = parser.expect_kind(Token.Operator)
            else:
                kind = FunctionKind.Regular

        if parser.match('('):
            parameters = parser.gather_objects([Parameter], ',')
            parser.expect(')')
        else:
            parameters = []

        semantic = parser.expect_kind(Token.Name) if parser.match(':') else None

        body = None
        if isTemplate:
            body = parser.fetch_block('{', '}')
        else:
            parser.expect('{')
            parser.expect('}')
        parser.match(';')
        
        # Register the function with the current namespace
        func = Function(userAnnotations, sysAnnotations, semantic, name, kind, returnTypename, extensionTargetName, parameters, body)
        parser.namespaceStack[-1].objects.append(func)

        return func

    @staticmethod
    def parse(parser):
        return Function.parse_template(parser, False)

    def generate_from_template(self, prettyString):
        # Emit the annotations
        prettyString += Annotation.usrlist_to_str(self.userAnnotations)
        prettyString += Annotation.syslist_to_str(self.sysAnnotations)

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
        prettyString += ' {'
        prettyString += '\n'
        prettyString.indentLevel += 1
        for t in self.body:
            prettyString += t.text

            # Change indentation
            if t.text == '{':
                prettyString += '\n'
                prettyString.indentLevel += 1
            elif t.text == '}':
                prettyString += '\n'
                prettyString.indentLevel -= 1
        prettyString += '\n'
        prettyString.indentLevel -= 1
        prettyString += '}'
        prettyString += '\n'

class Member(Named):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token, typename):
        super().__init__(userAnnotations, sysAnnotations, semantic, token)
        self.typename = typename

        if Annotation.is_not_compatible(['private', 'deprecate'], sysAnnotations):
            raise UnsupportedSystemAnnotationsError(self.token, 'Member', sysAnnotations)

    @staticmethod
    def parse(parser):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)
        typename = Typename.try_parse()
        if typename is None:
            return None

        name = parser.expect_kind(Token.Name)
        semantic = parser.expect_kind(Token.Name) if parser.match(':') else None
        
        parser.match(';')

        return Member(userAnnotations, sysAnnotations, semantic, name, typename)

class MemberList:
    @staticmethod
    def parse(parser):
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

        semantic = parser.expect_kind(Token.Name) if parser.match(':') else None

        parser.match(';')
        
        result = []
        for name in names:
            result.append(Member(userAnnotations, sysAnnotations, semantic, name, typename))
        
        return result

class Struct(Named):
    def __init__(self, userAnnotations, sysAnnotations, semantic, token, members, body):
        super().__init__(userAnnotations, sysAnnotations, semantic, token)
        self.members = members
        self.body = body

        if Annotation.is_not_compatible(['private', 'deprecate'], sysAnnotations):
            raise UnsupportedSystemAnnotationsError(self.token, 'Struct', sysAnnotations)

    @staticmethod
    def parse_template(parser, isTemplate):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        if not parser.match('struct'):
            return None

        token = parser.expect_kind(Token.Name)
        semantic = parser.expect_kind(Token.Name) if parser.match(':') else None

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
    def parse(parser):
        return Struct.parse_template(parser, False)

class Alias(Named):
    def __init__(self, userAnnotations, sysAnnotations, semantic, srcType, dstType):
        super().__init__(userAnnotations, sysAnnotations, semantic, srcType)
        self.srcType = srcType
        self.dstType = dstType

        if Annotation.is_not_compatible(['private', 'deprecate'], sysAnnotations):
            raise UnsupportedSystemAnnotationsError(self.token, 'Alias', sysAnnotations)

    @staticmethod
    def parse(parser):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)
        if not parser.match('using'):
            return None

        srcType = Typename.parse(parser)
        semantic = parser.expect_kind(Token.Name) if parser.match(':') else None
        parser.expect('{')
        dstType = Typename.parse(parser)
        parser.expect('}')
        
        # Register the struct with the current namespace
        alias = Alias(userAnnotations, sysAnnotations, semantic, srcType, dstType)
        parser.namespaceStack[-1].objects.append(alias)

        return alias

class Reference(Annotateable):
    def __init__(self, userAnnotations, sysAnnotations, string):
        super().__init__(userAnnotations, sysAnnotations)
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
        return ['static', 'private', 'noconstructor', 'deprecate', 'copyreferences', 'unitprefix', 'unitsuffix', 'prefix', 'suffix']

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
                token = parser.match_kind(Token.String)
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
    def parse(parser):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        name = parser.match_kind(Token.Name)
        if name is None:
            return None

        semantic = parser.expect_kind(Token.Name) if parser.match(':') else None

        return TemplateParameter(userAnnotations, sysAnnotations, semantic, name)

class Template(Named):
    def __init__(self, userAnnotations, sysAnnotations, semantic, parameters, obj, namespaceList, references):
        super().__init__(userAnnotations, sysAnnotations, semantic, obj.token)
        self.obj = obj
        self.parameters = parameters
        self.namespaceList = namespaceList
        self.namespaceList.pop(0)
        self.references = references

        if Annotation.is_not_compatible(['copyreferences', 'unitprefix', 'unitsuffix', 'prefix', 'suffix', 'private', 'deprecate'], sysAnnotations):
            raise UnsupportedSystemAnnotationsError(self.token, 'Template', sysAnnotations)

        Annotation.verify_annotation(self.sysAnnotations, 'unitprefix', [[Token.Literal.String]])
        Annotation.verify_annotation(self.sysAnnotations, 'unitsuffix', [[Token.Literal.String]])
        Annotation.verify_annotation(self.sysAnnotations,'prefix', [[Token.Literal.String]])
        Annotation.verify_annotation(self.sysAnnotations,'suffix', [[Token.Literal.String]])

    def generate_translation_unit(self):
        result = PrettyString()

        # References + Unitprefix
        for annotation in self.sysAnnotations:
            if annotation.token.text == 'copyreferences':
                for ref in self.references:
                    result += 'using {0};\n'.format(ref.string)
            elif annotation.token.text == 'unitprefix':
                result += '{0}\n'.format(annotation.args[0].text[1:-1])

        result += '\n'

        # Namespace
        for namespace in self.namespaceList:
            result += Annotation.usrlist_to_str(namespace.userAnnotations)
            result += Annotation.syslist_to_str(namespace.sysAnnotations)
            result += 'namespace {0}'.format(namespace.token.text)
            if namespace.semantic is not None:
                result += ': ' + namespace.semantic.text
            result += ' {\n'
            result.indentLevel += 1

        # Prefix
        for annotation in self.sysAnnotations:
            if annotation.token.text == 'prefix':
                result += annotation.args[0].text[1:-1] + '\n'

        # Emit the object string
        self.obj.generate_from_template(result)

        # Suffix
        for annotation in self.sysAnnotations:
            if annotation.token.text == 'suffix':
                result += annotation.args[0].text[1:-1] + '\n'

        # Close namespace
        for namespace in self.namespaceList:
            result.indentLevel -= 1
            result += "}\n"

        # Unit suffix
        for annotation in self.sysAnnotations:
            if annotation.token.text == 'unitsuffix':
                result += '{0}\n'.format(annotation.args[0].text[1:-1])

        return result.value.strip()

    @staticmethod
    def parse(parser):
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        if not parser.match('template'):
            return None

        parser.expect('<')
        parameters = parser.gather_objects([TemplateParameter], ',')
        parser.expect('>')
        semantic = parser.expect_kind(Token.Name) if parser.match(':') else None

        # Deduce the template kind
        templateClasses = [Struct, Function]
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

        # TEMPLATE TEST

        # Convert the template to a translation unit
        templateSource = template.generate_translation_unit()

        # Substitution table
        subs = {}
        subs['LHS'] = 'LHSTest'
        subs['RHS'] = 'RHSTest'
        tokens = parser.lexer.tokenize(templateSource, subs)

        # Dump pretty-formatted file
        templateSub = PrettyString.from_tokens(tokens)

        with io.open("D:\\test.txt", "w") as textFile:
            print(templateSub, file=textFile)

        return template