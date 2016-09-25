import re

from pygments.lexer import RegexLexer, include, bygroups, using, this, default, words
from pygments.util import get_bool_opt
from pygments.token import Token, Text, Operator, Name, String, Number, Punctuation, Error
from pygments.filter import simplefilter

__all__ = ['SymbolicLexer']

class Symto:

    def __init__(self, kind, fileName, text, line, column):
        
        self.kind = kind
        self.fileName = fileName
        self.text = text
        self.line = line
        self.column = column
        self.columnEnd = column + len(text)

class TokenValue:
    def __init__(self, fileName, text, line, column):
        self.text = text
        self.line = line
        self.column = column
        self.fileName = fileName

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

    tokens = {
        'root': [
            (r'[a-zA-Z_]\w*', Name),
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'\d+', Number.Integer),
            (r'(<<|<<=|>>|>>=|~=|\|=|&=|^=|\+=|\*=|%=|-=|==|!=|<=|>=|&&|\|\|)', Operator),
            (r'[@.~!%^&*+=|?:<>/-\[\]]', Operator),
            (r"'[^\']*'", String),
            (r'"[^\"]*"', String),
            (r'[,;(){}\\]', Punctuation),
            (r'\s', Text),
            (r'\n', Text),
        ],
    }

    def __init__(self, **options):
        RegexLexer.__init__(self, **options)
        self.fileName = ''

        # Register custom filter
        self.add_filter(symbolic_filter())

    def get_tokens_unprocessed(self, text):
        line = 1
        column = 1
        for index, token, value in RegexLexer.get_tokens_unprocessed(self, text):
            yield index, token, TokenValue(self.fileName, value, line, column)
            if value == '\n':
                line += 1
                column = 1
            else:
                column += len(value)

    def analyse_text(text):
        if re.search('using ', text):
            return 0.4
        return 0.0

    def tokenize(self, context):
        srcFileTokens = self.get_tokens(context)

        # Convert to custom token struct
        return [Symto(t[0], t[1].fileName, t[1].text, t[1].line, t[1].column) for t in srcFileTokens]