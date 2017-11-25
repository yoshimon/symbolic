"""Contains classes that can be used to parse symbolic source code."""

from pygments.token import Token, Name

from symbolic.base_parser import BaseParser
from symbolic.exceptions import ExpectedEOFError
from symbolic.lexer import Symto
from symbolic.objects import Namespace, Expression, Reference, Annotation, Struct, Function, Alias, Template

class UnitParser(BaseParser):
    """A parser for symbolic source files (units)."""

    def __init__(self, libName, fileName, tokens, preImports, postImports):
        """
        Initialize the object.

        Args:
            libName (str): The library name.
            fileName (str): The file name.
            tokens ([lexer.Symto]): The token list.
            preImports (list): The list of pre-import strings.
            postImports (list): The list of post-import strings.
        """
        super().__init__(libName, fileName, tokens)
        self.preImports = preImports
        self.postImports = postImports

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

    def gather_objects(self, classes, matchAfterSuccess=None, args=None, firstMatchOnly=False):
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
        args = [] if args is None else args
        while True:
            o = self.try_parse_any(classes, args)
            if o is None:
                break

            if isinstance(o, list):
                for e in o:
                    result.append(e)
            else:
                result.append(o)

            if matchAfterSuccess is not None:
                if not self.match(matchAfterSuccess):
                    break
            if firstMatchOnly:
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
        """Parse all library references."""
         # Add library name.
        self.references = [Reference(Symto(Token.Text, self.libName, self.fileName, self.libName, 1, 1), [], None)] if self.libName is not None else []

        # Add post-imports.
        self.references += [Reference(Symto(Token.Text, self.libName, self.fileName, postImport, 1, 1), [], None) for postImport in self.postImports] if self.postImports is not None else []

        while True:
            self.push_state()
            annotations = Annotation.parse_annotations(self)
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

            refStrList = [str(t) for t in refTokens]
            refString = '.'.join(refStrList)
            token = Symto.from_token(refTokens[0], Token.Token, refString)
            ref = Reference(token, annotations, semantic)

            # Reference sequences must be collapsed:
            # using A; using A; is equivalent to using A;
            if self.references and str(ref) != self.references[-1]:
                self.references.append(ref)

            self.remove_state()

        # Add pre-imports.
        self.references += [Reference(Symto(Token.Text, self.libName, self.fileName, preImport, 1, 1), [], None) for preImport in self.preImports] if self.preImports is not None else []

    def namespace(self):
        """
        Return the current namespace on top of the namespace stack.

        Returns:
            objects.Namespace or None: The namespace on top of the namespace stack or None, if no such element exists.
        """
        return self.namespaceStack[-1] if self.namespaceStack else None

    def parse_root_references_only(self):
        """
        Parse the beginning of the current token stream.

        This includes references and the root namespace setup.
        This will transform the parser into a "ready" state to issue
        manual parsing commands.
        """
        # Reset stream position
        self.reset(self.libName, self.fileName, self.tokens)

        # References first
        self.parse_all_references()

        # Initialize the root namespace.
        self.namespaceStack = [Namespace(self.references, None, Symto(Token.Text, self.libName, self.fileName, '', 1, 1), [], None)] # Global namespace

    def parse(self):
        """
        Parse the active token stream.

        The root does not have to manually parsed.

        Returns:
            objects.Namespace: The global (root) namespace object.
        """
        # Setup the parser.
        self.parse_root_references_only()

        # Parse, starting at the global namespace
        self.gather_namespace_objects()

        if not self.is_eof():
            raise ExpectedEOFError(self.token.anchor)

        return self.namespaceStack.pop()
