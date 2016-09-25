
class PrettyString:


    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.indentLevel = 0
        self.indentSize = 4 # in spaces
        self.value = ''

    def __iadd__(self, other):
        self.append(other)
        return self

    def indent_str(self):
        return ' ' * self.indentLevel * self.indentSize

    def append(self, text):
        # Indent every newline
        indentedText = text.replace('\n', '\n' + self.indent_str())

        if len(self.value) > 0:
            # With indent
            if self.value[-1] == '\n':
                self.value += self.indent_str() + indentedText
            else:
                # Without indent
                self.value += indentedText
        else:
            self.value = indentedText