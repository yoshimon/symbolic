"""Contains classes that can be used to parse source code."""

import re

from pygments.token import Token, Name

from symbolic.exceptions import UnexpectedEOFError, UnexpectedTokenError
from symbolic.lexer import Symto, SymbolicLexer

class BaseParser:
    """
    A base class for all parsers.

    Attributes:
        libName (str): The library name. This will be associated with the default EOF token.
        fileName (str): The file name. This will be associated with the default EOF token.
        tokens ([lexer.Symto]): The token list.
        tokenIdx (int): The token index.
        token (lexer.Symto): The symbolic token.
        tokenStateStack ([int]): A state stack for token indices.
        namespaceStack ([objects.Namespace]): The namespace stack.
    """

    def __init__(self, tokens):
        """
        Initialize the object.

        Args:
            libName (str): The library name. This will be associated with the default EOF token.
            fileName (str): The file name. This will be associated with the default EOF token.
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
            libName (str): The library name. This will be associated with the default EOF token.
            fileName (str): The file name. This will be associated with the default EOF token.
            tokens ([lexer.Symto]): The token stream.
        """
        self.libName = libName
        self.fileName = fileName

        self.tokens = list(tokens)

        # Start at first token
        self.tokenIdx = -1
        self.token = self.advance()

        self.tokenStateStack = []
        self.namespaceStack = []

    def advance(self):
        """
        Advance the parser by one token.

        Returns:
            lexer.Symto: The next token.
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
            lexer.Symto: The previous token.
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

    def match_name(self):
        """
        Match a name token.

        Returns:
            lexer.Symto: The token or None, if no matching token was found.
        """
        return self.match_kind(Token.Name)

    def match_name_optional(self):
        """
        Match a name token, returning the empty string if no match is found.

        Returns:
            lexer.Symto: The token or the empty string, if no matching token was found.
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
        Returns:
            list: The tokens up to the matched delimiter or the empty list.
        """
        if not endDelims:
            assert(False)

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
            lexer.Symto: The matched token or the default token, if matching failed.
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
            if str(val) == '<':
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
            assert(False)

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
        if str(val) == '>':
            if not wasPreviousString:
                self.back()
                return False

        # The indices into both tables have to match
        i = SymbolicLexer.openBrackets.index(str(stack[-1]))
        j = SymbolicLexer.closeBrackets.index(str(val))
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

    def fetch_block(self, startDelim, endDelims):
        """
        Fetch everything between two delimiters.

        Nested brackets will be ignored.

        Args:
            startDelim (str): The starting delimiter.
            endDelims ([str]): The ending delimiters.
        Returns:
            [lexer.Symto]: The block tokens.
        """
        self.push_state()

        tokens = []
        if self.match(startDelim):
            bracketStack = []
            while not self.is_eof() and (bracketStack or not self.match_any(endDelims)):
                wasBracketMatched = self.match_push_open_bracket(bracketStack, startDelim) or \
                                    any(self.match_pop_close_bracket(bracketStack, endDelim) for endDelim in endDelims)

                bracketLevel = len(bracketStack)
                if wasBracketMatched:
                    # Push the matched bracket.
                    t = Symto.with_bracket_level(self.previous(), bracketLevel)
                else:
                    # Push the next token.
                    t = Symto.with_bracket_level(self.consume(), bracketLevel)

                tokens.append(t)

            # Missing brackets or not fetching the end delimiter will result in failure.
            failed = bracketStack or (tokens and str(self.previous()) not in endDelims)
            if failed:
                self.pop_state()
                return []

            self.remove_state()
        else:
            self.pop_state()

        return tokens
