import re

from pygments.lexer import RegexLexer, include, bygroups, using, this, default, words
from pygments.util import get_bool_opt
from pygments.token import Token, Text, Operator, Name, String, Number, Punctuation, Error
from pygments.filter import simplefilter
from jinja2 import Template

__all__ = ['SymbolicLexer']

class Symto:
    @staticmethod
    def strlist(l, none=None):
        return map(lambda t: t.text if t is not None else none, l)

    @staticmethod
    def is_left_associtative(op):
        return (definitions[op][1])

    @staticmethod
    def is_right_associtative(op):
        return not is_left_associtative(op)

    @staticmethod
    def precedence(op):
        return definitions[op][0]

    @staticmethod
    def has_unary(op):
        return definitions[op][0]

    @staticmethod
    def from_token(other, kind, text):
        return Symto(kind, other.libName, other.fileName, text, other.line, other.column)

    def __init__(self, kind, libName, fileName, text, line, column):
        self.kind = kind
        self.libName = libName
        self.fileName = fileName
        self.text = text
        self.line = line
        self.column = column
        self.columnEnd = column + len(text)
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

    def __str__(self):
        return self.text

class TokenValue:
    def __init__(self, text, line, column):
        self.text = text
        self.line = line
        self.column = column

@simplefilter
def symbolic_filter(self, lexer, stream, options):
    for toktype, tokval in stream:
        if (toktype is Token.Text):
            continue
        yield toktype, tokval

class SymbolicLexer(RegexLexer):
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
        line = 1
        column = 1
        for index, token, value in RegexLexer.get_tokens_unprocessed(self, text):
            text = self.subs[value] if (self.subs is not None) and (value in self.subs) else value
            yield index, token, TokenValue(text, line, column)
            if value == '\n':
                line += 1
                column = 1
            else:
                column += len(text)

    def analyse_text(text):
        if re.search('using ', text):
            return 0.4
        return 0.0

    def tokenize(self, srcFileText, subs = None):
        # Bind substitution table
        self.subs = subs

        srcFileTxt = srcFileText.replace('$', self.libName)
        template = self.preprocessor.from_string(srcFileTxt, self.ppt, Template)
        context = template.render(self.ppt)
        srcFileTokens = self.get_tokens(context)
        actualTokens = [Symto(t[0], self.libName, self.fileName, t[1].text, t[1].line, t[1].column) for t in srcFileTokens]
        
        # Unbind substitution table
        self.subs = None
        
        return actualTokens

class Ops:
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