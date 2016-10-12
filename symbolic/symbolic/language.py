class Language:
    systemTypenameStrings = { 'void', 'float', 'int', 'bool', 'char', 'string' }
    keywords = { 'if', 'elif', 'else', 'return', 'for', 'while', 'do', 'break', 'continue' }
    invalidNames = systemTypenameStrings.union(keywords)