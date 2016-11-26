"""Contains classes to generate formatted strings."""

from symbolic.lexer import SymbolicLexer

class PrettyString:
    """
    A pretty-formatted string.
    
    Attributes:
        indentLevel (int): The indentation level.
        indentSize (int): The indentation size in spaces.
        value (str): The raw string value.
    """

    def __init__(self):
        """Initialize the object."""
        self.indentLevel = 0
        self.indentSize = 4 # in spaces
        self.value = ''

    def __iadd__(self, other):
        """
        Append another PrettyString to this object.

        Args:
            other (PrettyString): The other PrettyString.
        Returns:
            PrettyString: The combined string.
        """
        self.append(other)
        return self

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return self.value

    def indent_str(self):
        """
        Return the string that is used for indenting at the current level.
        
        Returns:
            str: The indentation string.
        """
        return ' ' * self.indentSize * self.indentLevel

    def append(self, text):
        """
        Append a textblock to the string, using pretty-formatting rules.
        
        Args:
            text (str): The text to append.
        Returns:
            PrettyString: The object itself.
        """
        if len(text) == 0:
            return self

        firstChar = text[0]

        if len(self.value) > 0:
            lastChar = self.value[-1]
            # Insert an additional indent at the beginning if newline
            if lastChar == '\n':
                self.value += self.indent_str()
            else:
                # Insert space if matching style
                if (str.isalnum(lastChar) and str.isalnum(firstChar)) or (lastChar == ',' and str.isalnum(firstChar)):
                    self.value += ' '
        
        # Replace mid-string newlines
        newText = ''
        for i in range(0, len(text)-1):
            c = text[i]
            newText += c
            if c == '\n':
                newText += self.indent_str()
        newText += text[-1]

        self.value += newText
        return self

    @staticmethod
    def from_tokens(tokens, firstLine=1):
        """
        Create a PrettyString from a token stream.

        Args:
            tokens (list of symbolic.lexer.Symto): The token stream.
            firstLine (int): The first line.
        Returns:
            PrettyString: The PrettyString object.
        """
        result = ''
        previousLine = firstLine
        previousColumnEnd = 1
        for t in tokens:
            if t.line != previousLine:
                result += '\n' * (t.line - previousLine)
                previousColumnEnd = 1

            result += ' ' * (t.column - previousColumnEnd)

            # Now append the token
            result += t.text
            previousLine = t.line
            previousColumnEnd = t.columnEnd
        return result