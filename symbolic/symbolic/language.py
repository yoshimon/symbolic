class Language:
    '''A collection of Symbolic language features.'''
    systemTypenameStrings = { 'void', 'float', 'int', 'bool' }
    keywords = { 'if', 'elif', 'else', 'return', 'for', 'while', 'do', 'break', 'continue' }
    invalidNames = systemTypenameStrings.union(keywords)