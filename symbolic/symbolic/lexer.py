"""Contains lexing-related classes."""

# Built-in
import re
from collections import deque

# Library
from pygments.lexer import RegexLexer, include, bygroups, using, this, default, words
from pygments.token import Token, Text, Operator, Name, String, Number, Punctuation, Error
from pygments.filter import simplefilter

# Project
from symbolic.language import Language

class Anchor:
    """
    An anchor in the source code.

    Attributes:
        libName (str): The library name.
        fileName (str): The file name.
        line (int): The line in the source code.
        column (int): The column in the source code.
    """

    def __init__(self, libName, fileName, line, column):
        """
        Initialize the object.

        Args:
            libName (str): The library name.
            fileName (str): The file name.
            line (int): The line in the source code.
            column (int): The column in the source code.
        """
        self.libName = libName
        self.fileName = fileName
        self.line = line
        self.column = column

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        fileName = "'{0}'".format(self.fileName) if self.fileName else ""
        libName = (" in {0}" if fileName else "In {0}").format(self.libName) if self.libName else ""
        line = " @ ({0}, {1})".format(self.line, self.column) if (fileName or libName) and self.line and self.column else ""
        total = fileName + libName + line
        return total

class Symto:
    """
    A symbolic token.
    
    Args:
        isOp (bool): Indicates whether the token is an operator.
        isBinaryOp (bool): Indicates whether the token is a binary operator.
        isBinaryLeftAssociative (bool): Indicates whether the associated binary operator is left-associative.
        isBinaryRightAssociative (bool): Indicates whether the associated binary operator is right-associative.
        binaryPrecedence (int): The binary operator precedence value.
        isUnaryOp (bool): Indicates whether the token is a unary operator.
        isUnaryLeftAssociative (bool): Indicates whether the associated unary operator is left-associative.
        isUnaryRightAssociative (bool): Indicates whether the associated unary operator is right-associative.
        unaryPrecedence (int): The unary operator precendence value.
        isOpenBracket (bool): Indicates whether the token is an opening bracket.
        matchingCloseBracket (str): The matching closing bracket, if the token is an opening bracket.
        isCloseBracket (bool): Indicates whether the token is a closing bracket.
        matchingOpenBracket (str): The matching opening bracket, if the token is a closing bracket.
    """

    @staticmethod
    def strlist(l, none=None):
        """
        Converts a token list to a string list.

        Args:
            l ([lexer.Symto]): The token list.
            none: Optional type to return for the None token.
        Returns:
            [str]: The string list.
        """
        return map(lambda t: t.text if t is not None else none, l)

    @staticmethod
    def join(l, c='.', none=None):
        """
        Join operation on a token list.

        Args:
            l ([lexer.Symto]): The token list.
            c (str): The join-string.
            none: Optional type to return for the None token.
        Returns:
            [str]: The string list.
        """
        return c.join(Symto.strlist(l, none))

    @staticmethod
    def from_token(other, kind, text):
        """
        Create a token from an existing token.

        Args:
            other (lexer.Symto): The other token.
            kind (pygments.token.Token): The token kind.
            text (str): The token text.
        Returns:
            Symto: The token.
        """
        return Symto(kind, other.anchor.libName, other.anchor.fileName, text, other.anchor.line, other.anchor.column)

    @staticmethod
    def after_token(other, kind, text):
        """
        Create a token from an existing token.

        Args:
            other (lexer.Symto): The other token.
            kind (pygments.token.Token): The token kind.
            text (str): The token text.
        Returns:
            Symto: The token.
        """
        return Symto(kind, other.anchor.libName, other.anchor.fileName, text, other.anchor.line, other.anchor.column + len(text) + 1)

    def __init__(self, kind, libName, fileName, text, line, column):
        """
        Initialize the object.

        Args:
            kind (pygments.token.Token): The token kind.
            libName (str): The library name.
            fileName (str): The file name.
            text (str): The text value.
            line (int): The line in which the token was lexed.
            column (int): The column in which the token was lexed.
        """
        self.kind = kind
        self.text = str(text)
        self.anchor = Anchor(libName, fileName, line, column)
        self.isTerminal = self.kind in [Token.Number.Float, Token.Number.Integer, Token.Number.Hex, Token.Name, Token.Literal.String]
        self.isNumber = self.kind in [Token.Number.Float, Token.Number.Integer, Token.Number.Hex]
        self.isInteger = self.kind in [Token.Number.Integer, Token.Number.Hex]
        self.isFloat = self.kind == Token.Number.Float
        
        # Op (for convenience)
        self.isOp = kind == Token.Operator
        if self.isOp:
            self.isBinaryOp = text in Ops.binary
            if self.isBinaryOp:
                op = Ops.binary[text]
                self.isBinaryLeftAssociative = op[1]
                self.isBinaryRightAssociative = not self.isBinaryLeftAssociative
                self.binaryPrecedence = op[0]
            
            self.isUnaryOp = text in Ops.unary
            if self.isUnaryOp:
                op = Ops.unary[text]
                self.isUnaryLeftAssociative = op[1]
                self.isUnaryRightAssociative = not self.isUnaryLeftAssociative
                self.unaryPrecedence = op[0]

        self.isOpenBracket = text in SymbolicLexer.openBrackets
        if self.isOpenBracket:
            self.matchingCloseBracket = SymbolicLexer.closeBrackets[SymbolicLexer.openBrackets.index(text)]

        self.isCloseBracket = text in SymbolicLexer.closeBrackets
        if self.isCloseBracket:
            self.matchingOpenBracket = SymbolicLexer.openBrackets[SymbolicLexer.closeBrackets.index(text)]

    def update(self, other, kind, text):
        """
        Update the token.

        Args:
            other (lexer.Symto): The other token to use as a baseline.
            kind (pygments.token.Token): The token kind.
            text (str): The token text.
        """
        self.__init__(kind, other.anchor.libName, other.anchor.fileName, text, other.anchor.line, other.anchor.column)

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return self.text

    def __eq__(self, other):
        """
        Return whether the token equals another token.

        Args:
            other (lexer.Symto or None): The other token.
        Returns:
            bool: True, if the tokens are equal. Otherwise False.
        """
        return (other != None) and (str(self) == str(other))

@simplefilter
def _symbolic_filter(self, lexer, stream, options):
    """
    Generator to filter text tokens.

    Args:
        lexer: Unused.
        stream: The token stream.
        options: Unused.
    """
    for toktype, tokval in stream:
        if toktype is not Token.Text:
            yield toktype, tokval

class SymbolicLexer(RegexLexer):
    """
    A lexer for Symbolic tokens.
    
    Attributes:
        name (str): The name of the lexer.
        aliases ([str]): The lexer aliases.
        filenames ([str]): The symbolic extensions.
        priority (float): The lexer priority.
        openBrackets ([str]): A list of all opening brackets.
        closeBrackets ([str]): A list of all closing brackets.
        tokens (dict): A regular expression state machine.

        fileName (str): The file name.
        libName (str): The library name.
        subs (dict): The template substitution table.
    """

    name = 'Symbolic'
    aliases = ['sym', 'symbolic']
    filenames = ['*.sym']
    priority = 0.1

    openBrackets = ['(', '{', '<', '[']
    closeBrackets = [')', '}', '>', ']']

    tokens = {
        'root': [
            (Language.tokenConcatenation, Punctuation),
            (r'[a-zA-Z_]\w*', Name),
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'\d+', Number.Integer),
            (r'(:=|<<|<<=|>>|>>=|~=|\|=|&=|^=|\+=|\*=|%=|-=|==|!=|<=|>=|&&|\|\|)', Operator),
            (r'[@.~!%^&*+-=|?:<>/]', Operator),
            (r"'[^\']*'", String),
            (r'"[^\"]*"', String),
            (r'[,;()\[\]{}\\]', Punctuation),
            (r'\s', Text),
            (r'\n', Text),
            (r'0[a-zA-Z]+', Error)
        ],
    }

    def __init__(self, libName=None, fileName=None, **options):
        """
        Initialize the object.

        Args:
            libName (str): The library name.
            fileName (str): The file name.
            options: The pygments lexer options.
        """
        RegexLexer.__init__(self, **options)
        # State must be maintained on the outside
        self.libName = libName
        self.fileName = fileName
        self.subs = None # Template / Substitution table

        # Register custom filter
        self.add_filter(_symbolic_filter())

    def get_tokens_unprocessed(self, text):
        """
        Generate an unprocessed token stream.

        Args:
            text (str): Unused.
        """

        class _TokenValue:
            """
            An intermediate representation of a token value.
            
            Attributes:
                text (str): The text.
                line (int): The line.
                column (int): The column.
            """

            def __init__(self, text, line, column):
                """
                Initialize the object.

                Args:
                    text (str): The text.
                    line (int): The line.
                    column (int): The column.
                """
                self.text = text
                self.line = line
                self.column = column

        # The member contains the right, unmodified text.
        text = self._unmodifiedText

        # Keeps track of the texts that still need processing.
        line = 1
        column = 1

        # Break it up into tokens.
        for _, kind, value in RegexLexer.get_tokens_unprocessed(self, text):
            # Perform substitution.
            # Either regular or with string ("") packing.
            wasSubstituted = False
            unpackedValue = value
            if self.subs:
                wasSubstituted = value in self.subs
                if wasSubstituted:
                    newValue = self.subs[value]
                elif kind == Token.String:
                    unpackedValue = value[1:-1]
                    wasSubstituted = unpackedValue in self.subs
                    if wasSubstituted:    
                        # Repack the new value.
                        newValue = '"{0}"'.format(self.subs[unpackedValue])

            if wasSubstituted:
                # Prevent recursive substitution by deleting the key.
                newSubs = dict(self.subs)
                del newSubs[unpackedValue]

                # Generate the tokens recursively.
                lex = SymbolicLexer(self.libName, self.fileName)
                newTokens = lex.tokenize(newValue, newSubs)
            else:
                newTokens = [Symto(kind, self.libName, self.fileName, value, 1, 1)]

            # Emit all new tokens
            for t in newTokens:
                # Return the unpacked token.
                # NOTE: packing / unpacking is required to work nicely with pygments.
                yield 0, t.kind, _TokenValue(t.text, line, column)
            
                # Adjust line and column counters
                if t.text == '\n':
                    line += 1
                    column = 1
                else:
                    column += len(t.text)

    def analyse_text(text):
        """
        Unused.

        Args:
            text: Unused.
        """
        return 0.0

    def tokenize(self, text, subs=None):
        """
        Tokenize a piece of text.

        Args:
            text (str): The text to tokenize.
            subs ({str, str}): The token substitution table.
        """
        # NOTE: this fixes an issue with pygments which modifies the text
        tokens = self.get_tokens(text)

        # NOTE: this is used by the generator below
        # Bind substitution table
        # Bind unmodified text - pygments modifies the text that is passed in
        self.subs = subs
        self._unmodifiedText = text
        # Convert pygments tokens to Symbolic tokens
        tokens = [Symto(t[0], self.libName, self.fileName, t[1].text, t[1].line, t[1].column) for t in tokens]
        self._unmodifiedText = None
        self.subs = None
        
        return tokens

    @staticmethod
    def concatenate_tokens(tokens):
        """
        Concatenate two tokens if separated by the concatenation operator.

        Args:
            tokens ([lexer.Symto]): The token sequence to process.
        Returns:
            [lexer.Symto]: The new token sequence.
        """
        newTokens = []
        skipNext = False
        for i, t in enumerate(tokens):
            if skipNext:
                skipNext = False
                continue

            if t.text == Language.tokenConcatenation:
                # Concat if possible.
                if (i > 0) and (i < len(tokens) - 1):
                    newTokens[-1] = Symto.from_token(tokens[i-1], Token.Name, tokens[i-1].text + tokens[i+1].text)
                    skipNext = True
            else:
                newTokens.append(t)

        return newTokens

class Ops:
    """
    Symbolic operators.
    
    Attributes:
        unary ({str, [int, bool]}): The unary operators.
            The dictionary maps unary operators to a precendence value and a left-associativity flag.
        binary ({str, [int, bool]}): The binary operators.
            The dictionary maps binary operators to a precendence value and a left-associativity flag.
    """

    unary = {
            # Name: Precedence, Left-associative
            '+': [2, False],
            '-': [2, False],
            '!': [2, False],
            '~': [2, False],
            '*': [2, False],
            '&': [2, False],
            '++': [2, False],
            }

    binary = {
            # Name: Precedence, Left-associative
            '.': [1, True],

            '*': [3, True],
            '/': [3, True],
            '%': [3, True],

            '+': [4, True],
            '-': [4, True],

            '<<': [5, True],
            '>>': [5, True],

            '<': [6, True],
            '<=': [6, True],
            '>': [6, True],
            '>=': [6, True],

            '==': [7, True],
            '!=': [7, True],

            '&': [8, True],
            
            '^': [9, True],
            
            '|': [10, True],
            
            '&&': [11, True],
            
            '||': [12, True],
            
            ':=': [14, False],
            '=': [14, False],
            '+=': [14, False],
            '-=': [14, False],
            '*=': [14, False],
            '/=': [14, False],
            '%=': [14, False],
            '<<=': [14, False],
            '>>=': [14, False],
            '&=': [14, False],
            '^=': [14, False],
            '|=': [14, False],
        }