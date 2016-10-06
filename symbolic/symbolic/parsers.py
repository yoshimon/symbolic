import networkx as nx
from pygments.token import Token, Name

from symbolic.objects import Namespace, Expression, Reference, Annotation, Struct, Function, Alias, Template
from symbolic.exceptions import UnexpectedTokenError, UnexpectedEOFError, UnsupportedSystemAnnotationsError
from symbolic.lexer import TokenValue, Symto, SymbolicLexer
import re

class Symto:
    def __init__(self, kind, text, line, column):
        self.kind = kind
        self.text = text
        self.line = line
        self.column = column

class BaseParser:
    def __init__(self, lexer, tokens):
        self.reset(lexer, tokens)

    def is_eof(self):
        return self.tokenIdx >= len(self.tokens)

    def reset(self, lexer, tokens):
        self.lexer = lexer
        self.tokens = list(tokens)

        self.tokenStateStack = []
        self.namespaceStack = [Namespace([], [], '', [Token.Text, '$Global'])]

        # Start at first token
        self.tokenIdx = -1
        self.token = self.advance()

    def advance(self):
        if self.is_eof():
            raise UnexpectedEOFError()

        self.tokenIdx += 1
        if not self.is_eof():
            self.token = self.tokens[self.tokenIdx]
        else:
            self.token = None
        return self.token

    def can_back(self):
        return self.tokenIdx > 0

    def back(self):
        if self.tokenIdx == 0:
            raise UnexpectedEOFError()

        self.tokenIdx -= 1
        if self.tokenIdx >= 0:
            self.token = self.tokens[self.tokenIdx]
        else:
            self.token = None
        return self.token

    def consume(self):
        if self.is_eof():
            raise UnexpectedEOFError()

        token = self.token
        self.advance()
        return token

    def match(self, value):
        if self.is_eof():
            return False

        success = value == self.token.text
        if success:
            self.advance()
        return success

    def match_any(self, list):
        for val in list:
            token = self.token
            if self.match(val):
                return token

        return None

    def match_map(self, default, kvList):
        for (k, v) in kvList:
            if self.match(k):
                return v

        return default

    def peek_kind(self, tokenType):
        return self.peek_kinds([tokenType])

    def peek_kinds(self, tokenTypes):
        token = self.token
        success = token.kind in tokenTypes
        if success:
            return token
        else:
            return None

    def match_kind(self, tokenType):
        return self.match_kinds([tokenType])

    def match_kinds(self, tokenTypes):
        token = self.peek_kinds(tokenTypes)
        if token is not None:
            self.advance()
        return token

    def expect(self, val):
        if not self.match(val):
            raise UnexpectedTokenError(self.token, val, self.token.text)
        return self.token

    def expect_kind(self, tokenType):
        token = self.match_kind(tokenType)
        if token is None:
            raise UnexpectedTokenError(self.token, tokenType, self.token.text)
        return token

    def push_state(self):
        self.tokenStateStack.append(self.tokenIdx)

    def pop_state(self):
        self.tokenIdx = self.remove_state() - 1
        self.advance()

    def remove_state(self):
        return self.tokenStateStack.pop()

    def until_any(self, endDelims):
        if not endDelims:
            raise AssertionError()

        bracketStack = []
        result = []
        while len(bracketStack) > 0 or not self.match_any(endDelims):
            if self.match_push_any_open_bracket(bracketStack):
                result.append(bracketStack[-1])
            else:
                bracket = self.token
                if self.match_pop_matching_close_bracket(bracketStack):
                    result.append(bracket)
                else:
                    result.append(self.consume())

        # Dont include the end delimiter
        self.back()

        return result

    def match_push_any_open_bracket(self, stack):
        val = self.match_any(SymbolicLexer.openBrackets)
        success = val is not None
        if success:
            # Make sure < operators are not confused with template brackets
            if val.text == '<':
                if not self.peek_kind(Token.Literal.String):
                    self.back()
                    return False

            stack.append(val)
        return success

    def match_push_open_bracket(self, stack, openBracket):
        val = self.token
        success = self.match(openBracket)
        if success:
            stack.append(val)
        return success

    def was_previous_match_kind(self, kind):
        if not self.can_back():
            return False

        self.back()
        success = self.match_kind(kind)
        if not success:
            self.consume()
        return success

    def match_pop_matching_close_bracket(self, stack):
        if len(stack) == 0:
            return False

        # Template disambiguation
        wasPreviousString = self.was_previous_match_kind(Token.Literal.String)

        val = self.match_any(SymbolicLexer.closeBrackets)
        
        if val is None:
            return False

        # Make sure > operators are not confused with template brackets
        if val.text == '>':
            if not wasPreviousString:
                self.back()
                return False

        # The indices into both tables have to match
        i = SymbolicLexer.openBrackets.index(stack[-1].text)
        j = SymbolicLexer.closeBrackets.index(val.text)
        success = i == j
        if success:
            stack.pop()
        return success

    def match_pop_close_bracket(self, stack, closeBracket):
        if len(stack) == 0:
            return False

        val = self.token
        success = self.match(closeBracket)
        if success:
            stack.pop()

        return success

    def fetch_block(self, startDelim, endDelim):
        tokens = []
        if self.match(startDelim):
            bracketStack = []
            while len(bracketStack) > 0 or not self.match(endDelim):
                if self.match_push_open_bracket(bracketStack, startDelim):
                    tokens.append(bracketStack[-1])
                else:
                    bracket = self.token
                    if self.match_pop_close_bracket(bracketStack, endDelim):
                        tokens.append(bracket)
                    else:
                        tokens.append(self.consume())
        return tokens

class UnitParser(BaseParser):
    def __init__(self, lexer, tokens):
        super().__init__(lexer, tokens)
        # Create a dependency graph for this unit with the global namespace
        self.unitGraph = nx.DiGraph()
        self.unitGraph.add_node('$Global')

    def try_parse_any(self, classes, args):
        ''' Parses all object types in the supplied list. '''
        if self.is_eof():
            return None

        for c in classes:
            self.push_state()
            o = c.parse(self, args)
            if o is not None:
                self.remove_state()
                return o
            self.pop_state()

        return None

    def gather_objects(self, classes, matchAfterSuccess = None, args = []):
        result = []
        while True:
            o = self.try_parse_any(classes, args)
            if o is None:
                break
            result.append(o)
            if matchAfterSuccess is not None:
                if not self.match(matchAfterSuccess):
                    break
        return result

    def gather_namespace_objects(self):
        # NOTE: Function should always come last
        return self.gather_objects([Namespace, Struct, Alias, Template, Function])

    def parse_all_references(self):
        ''' Parses all library references. '''
        # using statements
        references = []

        while True:
            self.push_state()
            userAnnotations, sysAnnotations = Annotation.parse_annotations(self)
            if not self.match('import'):
                self.pop_state()
                break

            # The first token has to be a name
            refTokens = [self.expect_kind(Token.Name)]

            # Followed by optional suffixes .name
            while self.match('.'):
                refTokens.append(self.expect_kind(Token.Name))

            semantic = Annotation.parse_semantic(self)
            self.expect(';')

            refStrList = [t.text for t in refTokens]
            refString = '.'.join(refStrList)
            ref = Reference(userAnnotations, sysAnnotations, semantic, refString)
            references.append(ref)

        return references

    def parse(self):
        ''' Converts the token stream to an AST. '''
        # Reset stream position
        self.reset(self.lexer, self.tokens)

        # References first
        self.references = self.parse_all_references()

        # Parse, starting at the global namespace
        self.gather_namespace_objects()

        return self.references, self.namespaceStack.pop()