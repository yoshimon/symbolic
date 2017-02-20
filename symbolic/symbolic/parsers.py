"""Contains classes that can be used to parse symbolic source code."""

# Built-in
import re

# Library
from pygments.token import Token, Name

# Project
from symbolic.exceptions import *
from symbolic.objects import Namespace, Expression, Reference, Annotation, Struct, Function, Alias, Template
from symbolic.lexer import Symto, SymbolicLexer

class BaseParser:
    """
    A base class for all parsers.
    
    Attributes:
        libName (str): The library name.
        fileName (str): The file name.
        tokens ([lexer.Symto]): The token list.
        tokenIdx (int): The token index.
        token (lexer.Symto): The symbolic token.
        tokenStateStack ([int]): A state stack for token indices.
        namespaceStack ([objects.Namespace]): The namespace stack.
    """

    def __init__(self, libName, fileName, tokens):
        """
        Initialize the object.

        Args:
            tokens ([lexer.Symto]): The token list.
        """
        self.reset(libName, fileName, tokens)

    def is_eof(self):
        """
        Return whether the EOF is reached.

        Returns:
            bool: True, if the EOF is reached. Otherwise False.
        """
        return self.tokenIdx >= len(self.tokens)

    def reset(self, libName, fileName, tokens):
        """
        Reset the parser.

        Args:
            tokens ([lexer.Symto]): The token stream.
        """
        self.libName = libName
        self.fileName = fileName

        self.tokens = list(tokens)

        # Start at first token
        self.tokenIdx = -1
        self.token = self.advance()

        self.tokenStateStack = []
        self.namespaceStack = [Namespace(None, Symto(Token.Text, libName, fileName, '', 1, 1), [], [], None)] # Global namespace

    def advance(self):
        """
        Advance the parser by one token.

        Returns:
            lexer.Symto: The new token.
        """
        if self.is_eof():
            raise UnexpectedEOFError(self.token.anchor)

        self.tokenIdx += 1
        if not self.is_eof():
            self.token = self.tokens[self.tokenIdx]
        else:
            if self.can_back():
                # After the last token
                self.token = Symto.after_token(self.token, Token.EOF, "")
            else:
                # Empty file
                self.token = Symto(Token.EOF, self.libName, self.fileName, "", 1, 1)
        return self.token

    def can_back(self):
        """
        Return whether the parser can go back one token.

        Returns:
            bool: True, if the parser can go back. Otherwise False.
        """
        return self.tokenIdx > 0

    def back(self):
        """
        Step the parser back by one token.
        
        Returns:
            lexer.Symto: The new token.
        """
        if self.tokenIdx == 0:
            raise UnexpectedEOFError(self.token.anchor)

        self.tokenIdx -= 1
        if self.tokenIdx >= 0:
            self.token = self.tokens[self.tokenIdx]
        else:
            self.token = None
        return self.token

    def consume(self):
        """
        Return the current token and advance the parser.

        Returns:
            lexer.Symto: The current token.
        """
        if self.is_eof():
            raise UnexpectedEOFError(self.token.anchor)

        token = self.token
        self.advance()
        return token

    def match(self, value):
        """
        Match the current token to a value.
        
        Args:
            value (str): The value to match.
        Returns:
            lexer.Symto: The next token.
        """
        if self.is_eof():
            return False

        success = value == str(self.token)
        if success:
            self.advance()
        return success

    def match_any(self, sequence):
        """
        Match any value in a given list of values.
        
        Args:
            sequence ([str]): A sequence of string values to match.
        Returns:
            lexer.Symto: The next token.
        """
        for val in sequence:
            token = self.token
            if self.match(val):
                return token

        return None

    def match_map(self, kvSequence, default):
        """
        Match any value in a given sequence of values and maps it to a different value.

        Args:
            kvSequence: The (key, value)-sequence to match against.
            default: The default value to return.
        Returns:
            The mapped value or the default value, if no match is found.
        """
        for (k, v) in kvSequence:
            if self.match(k):
                return v

        return default

    def peek_kind(self, tokenType):
        """
        Peek the next token, if it matches a kind.

        Args:
            tokenType: The token type.
        Returns:
            lexer.Symto: The token or None if no matching token was found.
        """
        return self.peek_kinds([tokenType])

    def peek_kinds(self, tokenTypes):
        """
        Peek the next token, if it is within a specified set of kinds.

        Args:
            tokenTypes (list): The token types.
        Returns:
            lexer.Symto: The token or None if no matching token was found.
        """
        token = self.token
        success = token.kind in tokenTypes
        if success:
            return token
        else:
            return None

    def match_kind(self, tokenType):
        """
        Match a token, if its type matches a specified value.

        Args:
            tokenType: The token type.
        Returns:
            lexer.Symto: The token or None, if no matching token was found.
        """
        return self.match_any_kind([tokenType])

    def match_name_optional(self):
        """
        Match a name token, optionally returning the empty string.

        Returns:
            lexer.Symto: The token or an empty string token, if no matching name token was found.
        """
        return self.match_kind_optional(Token.Name, Token.Name, "")

    def match_any_kind(self, tokenTypes):
        """
        Match a token, if its type is contained in a specified type list.

        Args:
            tokenTypes (list): The token types.
        Returns:
            lexer.Symto: The token or None, if no matching token was found.
        """
        token = self.peek_kinds(tokenTypes)
        if token is not None:
            self.advance()
        return token

    def expect(self, val):
        """
        Expect the next token to have a given text value.

        Args:
            val (str): The expected text value.
        Returns:
            lexer.Symto: The next token.
        """
        if self.is_eof():
            raise UnexpectedEOFError(self.token.anchor)

        if not self.match(val):
            raise UnexpectedTokenError(self.token.anchor, val, str(self.token))

        return self.token

    def expect_kind(self, tokenType):
        """
        Expect the next token to be of a specific kind.

        Args:
            tokenType: The expected token kind.
        Returns:
            lexer.Symto: The token.
        """
        token = self.match_kind(tokenType)
        if token is None:
            raise UnexpectedTokenError(self.token.anchor, tokenType, str(self.token))
        return token

    def push_state(self):
        """Push a state."""
        self.tokenStateStack.append(self.tokenIdx)

    def pop_state(self):
        """Pop the last saved state."""
        self.tokenIdx = self.remove_state() - 1
        self.advance()

    def remove_state(self):
        """
        Remove the last pushed state without entering it.
        
        Returns:
            int: The token index that would have been restored.
        """
        return self.tokenStateStack.pop()

    def until_any(self, endDelims):
        """
        Fetch all tokens until an end delimiter is encountered.

        Args:
            endDelims ([str]): A list of end delimiters.
        """
        if not endDelims:
            raise DevError()

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

    def match_kind_optional(self, kind, optionalKind, optionalText):
        """
        Match a token or return a default token if unsuccessful.

        Args:
            kind (Token): The token kind to match.
            optionalKind (Token): The default token kind to use if no matching token is found.
            optionalText (str): The default token text to use if no matching token is found.
        Returns:
            lexer.Symto: 
        """
        token = self.match_kind(kind)
        return Symto.from_token(self.token, optionalKind, optionalText) if token is None else token

    def match_push_any_open_bracket(self, stack):
        """
        Match and push any opening bracket onto a bracket stack.

        Args:
            stack ([lexer.Symto]): The bracket stack.
        Returns:
            bool: True, if an opening bracket was matched and pushed. Otherwise False.
        """
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
        """
        Match and push an opening bracket onto a bracket stack.

        Args:
            stack ([lexer.Symto]): The bracket stack.
            openBracket (str): The opening bracket to match.
        Returns:
            bool: True, if the opening bracket was matched. Otherwise False.
        """
        val = self.token
        success = self.match(openBracket)
        if success:
            stack.append(val)
        return success

    def previous(self):
        """
        Return the previous token.

        Returns:
            lexer.Symto: The previous token or None, if no such token exists.
        """
        if not self.can_back():
            raise DevError()

        self.back()
        return self.consume()

    def was_previous_match_kind(self, kind):
        """
        Return whether the previous match was of a specified kind.

        Args:
            kind (Token): The token kind to match.
        Returns:
            bool: True, if the last matched token was of the specified kind. Otherwise False.
        """
        if not self.can_back():
            return False

        self.back()
        success = self.match_kind(kind)
        if not success:
            self.consume()
        return success

    def match_pop_matching_close_bracket(self, stack):
        """
        Match a closing bracket based on a bracket stack.

        Args:
            stack ([lexer.Symto]): The bracket stack.
        Returns:
            bool: True, if the matching bracket for the stack top was matched. Otherwise False.
        """
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
        """
        Match a closing bracket and modify a bracket-stack on success.

        Args:
            stack ([lexer.Symto]): The bracket stack.
            closeBracket (str): The closing bracket to match.
        Returns:
            bool: True, if the bracket was matched. Otherwise False.
        """
        if len(stack) == 0:
            return False

        success = self.match(closeBracket)
        if success:
            stack.pop()

        return success

    def fetch_block(self, startDelim, endDelim):
        """
        Fetch everything between two delimiters.

        Respects nested brackets.

        Args:
            startDelim (str): The starting delimiter.
            endDelim (str): The ending delimiter.
        Returns:
            [lexer.Symto]: The block tokens.
        """
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

class ParseResult:
    """
    The result of a parsing operation.

    Attributes:
        references ([objects.Reference]): The references.
        rootNamespace (objects.Namespace): The root namespace.
    """

    def __init__(self, references, rootNamespace):
        self.references = references
        self.rootNamespace = rootNamespace

class UnitParser(BaseParser):
    """A parser for symbolic source files (units)."""

    def try_parse_any(self, classes, args):
        """
        Parse all object types in the supplied list.
        
        Args:
            classes ([class]): A list of classes to try to parse.
            args: A list of arguments to forward to the object parsers.
        """
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
        """
        Gather a list of parsable objects.

        Args:
            classes ([class]): A list of classes to try to parse.
            matchAfterSuccess (str): The symbol to match after a successful gather.
            args: Arguments to forward to the class.
        Returns:
            object: The first match or None, if no object was gathered.
        """
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
        """
        Gather all objects in a namespace.
        
        Returns:
            [object]: The gathered objects.
        """
        # NOTE: Function should always come last
        return self.gather_objects([Namespace, Struct, Alias, Template, Function])

    def parse_all_references(self):
        """
        Parse all library references.
        
        Returns:
            [objects.Reference]: The reference list.
        """
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
            token = Symto.from_token(refTokens[0], Token.Token, refString)
            ref = Reference(token, userAnnotations, sysAnnotations, semantic)
            references.append(ref)

        return references

    def namespace(self):
        """
        Return the current namespace on top of the namespace stack.

        Returns:
            objects.Namespace: The namespace on top of the namespace stack.
        """
        return self.namespaceStack[-1]

    def parse(self):
        """
        Parse the active token stream.
        
        Returns:
            objects.ParseResult: The list of references and the global (root) namespace object.
        """
        # Reset stream position
        self.reset(self.libName, self.fileName, self.tokens)

        # References first
        self.references = self.parse_all_references()

        # Parse, starting at the global namespace
        self.gather_namespace_objects()

        if not self.is_eof():
            raise ExpectedEOFError(self.token.anchor)

        return ParseResult(self.references, self.namespaceStack.pop())