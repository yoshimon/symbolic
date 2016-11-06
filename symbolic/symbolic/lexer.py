# Built-in
import re

# Library
from pygments.lexer import RegexLexer, include, bygroups, using, this, default, words
from pygments.token import Token, Text, Operator, Name, String, Number, Punctuation, Error
from pygments.filter import simplefilter
from jinja2 import Template

__all__ = ['SymbolicLexer']

class Symto:
    '''A Symbolic token.'''
    @staticmethod
    def strlist(l, none=None):
        '''
        Converts a token list to a string list.

        Args:
            l (list(Symto)): The token list.
            none: Optional type to return for the None token.
        Returns:
            list(str): The string list.
        '''
        return map(lambda t: t.text if t is not None else none, l)

    @staticmethod
    def join(l, c='.', none=None):
        '''
        Join operation on a token list.

        Args:
            l (list(Symto)): The token list.
            c (str): The join-string.
            none: Optional type to return for the None token.
        Returns:
            list(str): The string list.
        '''
        return c.join(Symto.strlist(l, none))

    @staticmethod
    def is_left_associtative(op):
        '''
        Test whether an operator is left-associative.

        Args:
            op (str): The operator.
        Returns:
            bool: True, if the operator is left-associative. Otherwise false.
        '''
        return (definitions[op][1])

    @staticmethod
    def is_right_associtative(op):
        '''
        Test whether an operator is right-associative.

        Args:
            op (str): The operator.
        Returns:
            bool: True, if the operator is right-associative. Otherwise false.
        '''
        return not is_left_associtative(op)

    @staticmethod
    def precedence(op):
        '''
        Return the precende of an operator.
        
        Args:
            op (str): The operator.
        Returns:
            int: The precedence value.
        '''
        return definitions[op][0]

    @staticmethod
    def has_unary(op):
        '''
        Return whether an operator has a unary equivalent.
        
        Args:
            op (str): The operator.
        Returns:
            bool: True, if the operator has a unary counterpart. Otherwise false.
        '''
        return definitions[op][0]

    @staticmethod
    def from_token(other, kind, text):
        '''
        Create a token from an existing token.

        Args:
            other (Symto): The other token.
            kind (Token): The token kind.
            text (str): The token text.
        Returns:
            Symto: The token.
        '''
        return Symto(kind, other.libName, other.fileName, text, other.line, other.column)

    @staticmethod
    def after_token(other, kind, text):
        '''
        Create a token from an existing token.

        Args:
            other (Symto): The other token.
            kind (Token): The token kind.
            text (str): The token text.
        Returns:
            Symto: The token.
        '''
        return Symto(kind, other.libName, other.fileName, text, other.line, other.columnEnd + 1)

    def __init__(self, kind, libName, fileName, text, line, column):
        '''
        Initialize the object.

        Args:
            kinds (Token): The token kind.
            libName (str): The library name.
            libName (str): The library name.
            text (str): The text value.
            line (int): The line in which the token was lexed.
            column (int): The column in which the token was lexed.
        '''
        self.kind = kind
        self.libName = libName
        self.fileName = fileName
        self.text = str(text)
        self.line = line
        self.column = column
        self.columnEnd = column + len(self.text)
        self.isTerminal = self.kind in [Token.Number.Float, Token.Number.Integer, Token.Number.Hex, Token.Name, Token.Literal.String]
        self.isNumber = self.kind in [Token.Number.Float, Token.Number.Integer, Token.Number.Hex]
        
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
        '''
        Update the token.

        Args:
            other (Symto): The other token to use as a baseline.
            kind (Token): The token kind.
            text (str): The token text.
        '''
        self.__init__(kind, other.libName, other.fileName, text, other.line, other.column)

    def __str__(self):
        '''
        Return a string representation of the object.

        Returns:
            str: The string representation.
        '''
        return self.text

class TokenValue:
    def __init__(self, text, line, column):
        '''
        Initialize the object.

        Args:
            text (str): The text.
            line (int): The line.
            column (int): The column.
        '''
        self.text = text
        self.line = line
        self.column = column

@simplefilter
def symbolic_filter(self, lexer, stream, options):
    '''
    Generator to filter text tokens.

    Args:
        lexer: Unused.
        stream: The token stream.
        options: Unused.
    '''
    for toktype, tokval in stream:
        if toktype is not Token.Text:
            yield toktype, tokval

class SymbolicLexer(RegexLexer):
    '''A lexer for Symbolic tokens.'''
    name = 'Symbolic'
    aliases = ['sym', 'symbolic']
    filenames = ['*.sym']
    priority = 0.1

    openBrackets = ['(', '{', '<', '[']
    closeBrackets = [')', '}', '>', ']']

    tokens = {
        'root': [
            (r'[a-zA-Z_]\w*', Name),
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'0[a-zA-Z]+', Error),
            (r'\d+', Number.Integer),
            (r'(<<|<<=|>>|>>=|~=|\|=|&=|^=|\+=|\*=|%=|-=|==|!=|<=|>=|&&|\|\|)', Operator),
            (r'[@.~!%^&*+-=|?:<>/\[\]]', Operator),
            (r"'[^\']*'", String),
            (r'"[^\"]*"', String),
            (r'[,;(){}\\]', Punctuation),
            (r'\s', Text),
            (r'\n', Text),
        ],
    }

    def __init__(self, preprocessor, **options):
        '''
        Initialize the object.

        Args:
            preprocessor: The pre-processor to use.
            options: The pygments lexer options.
        '''
        RegexLexer.__init__(self, **options)
        # State must be maintained on the outside
        self.fileName = ''
        self.preprocessor = preprocessor
        self.libName = None
        self.ppt = None # Pre-processor table
        self.subs = None # Template / Substitution table

        # Register custom filter
        self.add_filter(symbolic_filter())

    def get_tokens_unprocessed(self, text):
        '''
        Generate an unprocessed token stream.

        Args:
            text (str): The text to tokenize.
        '''
        line = 1
        column = 1
        for index, token, value in RegexLexer.get_tokens_unprocessed(self, text):
            txt = self.subs[value] if (self.subs is not None) and (value in self.subs) else value
            yield index, token, TokenValue(txt, line, column)
            if value == '\n':
                line += 1
                column = 1
            else:
                column += len(txt)

    def analyse_text(text):
        '''
        Unused.

        Args:
            text: Unused.
        '''
        return 0.0

    def tokenize(self, srcFileText, subs=None):
        '''
        Tokenize a piece of text.

        Args:
            srcFileText (str): The source.
            subs (dict(str, str)): The token substitution table.
        '''
        # Bind substitution table
        self.subs = subs

        # Replace $ with library name
        srcFileTxt = srcFileText.replace('$', self.libName)

        # Run the pre-processor
        template = self.preprocessor.from_string(srcFileTxt, self.ppt, Template)
        context = template.render(self.ppt)
        srcFileTokens = self.get_tokens(context)
        
        # Convert pygments tokens to Symbolic tokens
        actualTokens = [Symto(t[0], self.libName, self.fileName, t[1].text, t[1].line, t[1].column) for t in srcFileTokens]
        
        # Unbind substitution table
        self.subs = None
        
        return actualTokens

class Ops:
    '''Symbolic operators.'''
    unary = {
            # Name: Precedence, Left-associative
            '+': [2, False],
            '-': [2, False],
            '!': [2, False],
            '~': [2, False],
            '*': [2, False],
            '&': [2, False],
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