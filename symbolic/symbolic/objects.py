"""
Contains all source code object types in symbolic.

The types in this package are used to convert the textual representation of the symbolic source code to an in-memory representation.
This is later passed onto the dependency solver to resolve library dependencies, see symbolic.dag.
"""

# Built-in
import io
from enum import Enum
from copy import deepcopy
from functools import wraps

# Library
from pygments.token import Token, Text, Operator, Name, String, Number, Punctuation, Error

# Project
from symbolic.exceptions import *
from symbolic.lexer import SymbolicLexer, Symto
from symbolic.formatter import PrettyString
from symbolic.algorithm import Algorithm
from symbolic.language import Language

class Decorators:
    """A collection of custom decorators."""

    @staticmethod
    def validated(f):
        """
        Validate the object after initialization.
        
        Args:
            f (function): The functor to invoke.
        """

        @wraps(f)
        def wrapper(self, *args, **kwargs):
            f(self, *args, **kwargs)
            self.validate()
        return wrapper

class LocationKind(Enum):
    """
    Enumeration of possible Location kinds.
    
    Attributes:
        Unresolved (int): The location kind has not been resolved yet.
        Function (int): The location is occupied by a Function.
        Type (int): The location is occupied by a Struct or Alias.
        Template (int): The location is occupied by a Template.
        Instruction (int): The location is occupied by a Instruction.
        Namespace (int): The location is occupied by a Namespace.
        Reference (int): The location is occupied by a Reference.
    """

    Unresolved = 0
    Function = 1
    Type = 2
    Template = 3
    Instruction = 4
    Namespace = 5
    Reference = 6

class RelativeLocation:
    """
    A relative location specifier.
    
    Multiple RelativeLocation objects can represent an absolute path (see Location).

    Attributes:
        kind (objects.LocationKind): The location type specifier.
        name (str): The location name.
        templateParameters (list of objects.TemplateParameter): The template parameters.
        parameters (list of objects.Parameter): The signature.
    """

    def __init__(self, kind, name, *, templateParameters=None, parameters=None):
        """Initialize the object.
        Args:
            kind (objects.LocationKind): The location type specifier.
            name (str): The location name.
            templateParameters (list of objects.TemplateParameter): The template parameters.
            parameters (list of objects.Parameter): The signature.
        """
        self.kind = kind
        self.name = name
        self.templateParameters = templateParameters if templateParameters is not None else []
        self.parameters = parameters if parameters is not None else []

    def is_plain(self):
        """
        Return whether the relative location is plain.
        
        A location is considered to be plain when it does not have any (template) parameters.

        Returns:
            bool: True, if the location is plain. Otherwise False.
        """
        return (not self.templateParameters) and (not self.parameters)

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation of the object.
        """
        templateStr = "<{0}>".format(Algorithm.join_comma(self.templateParameters)) if self.templateParameters else ""
        parameterStr = "({0})".format(Algorithm.join_comma(self.parameters)) if self.parameters else ""
        kindStr = " as {0}".format(str(self.kind)) if self.kind != LocationKind.Unresolved else ""
        return self.name + templateStr + parameterStr + kindStr

    def __eq__(self, other):
        """
        Compare for equality with another relative location.
        
        Args:
            other (objects.Location): The other relative location.
        Returns:
            bool: True, if the two locations are equal. Otherwise False.
        """
        return (self.kind == other.kind) and \
            (self.name == other.name) and \
            (self.parameters == other.parameters) and \
            (self.templateParameters == other.templateParameters)

class Location:
    """
    An (absolute) location within a library.
    
    Attributes:
        path (list of objects.RelativeLocation): A list of relative location specifiers.
    """

    def __init__(self, path):
        """
        Initialize the object.

        Args:
            path (list of objects.RelativeLocation): A list of relative location specifiers.
        """
        self.path = path

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return Algorithm.join_dot(self.path)

    def __eq__(self, other):
        """
        Compare for equality with another location.
        
        Args:
            other (objects.Location): The other location.
        Returns:
            bool: True, if the two locations are equal. Otherwise False.
        """
        return self.path == other.path

    def __getitem__(self, key):
        """
        Return a relative location in the location path.

        Args:
            key: The path key.
        Returns:
            objects.RelativeLocation: The relative location.
        """
        return self.path.__getitem__(key)

    def __setitem__(self, key, value):
        """
        Change a relative location in the location path.

        Args:
            key: The path key.
            value (objects.RelativeLocation): The value to set.
        Returns:
            objects.RelativeLocation: The relative location.
        """
        return self.path.__setitem__(key, value)

    def __iter__(self):
        """
        Return an iterator for the location path.

        Returns:
            objects.RelativeLocation: The relative location.
        """
        return self.path.__iter__()

    def __delitem__(self, key):
        """
        Delete a relative location in the location path.

        Args:
            key: The key to delete.
        """
        return self.path.__delitem__(key)

class Locatable:
    """
    A locatable object within a library.

    Multiple Locatable objects represent a hierarchy. An Anchor is used
    to locate the object within a library.
    
    Attributes:
        parent (objects.Locatable): The parent object.
        anchor (Token): The anchor in the source code.        
    """

    def __init__(self, parent, anchor):
        """
        Initialize the object.
        
        Args:
            parent (objects.Locatable): The parent object.
            anchor (Token): The anchor in the source code.
        """
        self.parent = parent
        self.anchor = anchor

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        # Force this to be implemented by all derived classes
        raise DevError()

    def class_name(self):
        """
        Return the class name of the object.

        Returns:
            str: The class name.
        """
        return self.__class__.__name__

class Named(Locatable):
    """
    A named object within a library.
    
    Attributes:
        token (lexer.Symto): A token which holds the name.
        userAnnotations (list of objects.Annotation): The user annotations.
        sysAnnotations (list of objects.Annotation): The system annotations.
        semantic (lexer.Symto): The semantic annotation.
    """

    def __init__(self, parent, token, userAnnotations, sysAnnotations, semantic):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
        """
        super().__init__(parent, token.anchor)
        assert token is not None
        assert userAnnotations is not None
        assert sysAnnotations is not None
        self.userAnnotations = userAnnotations
        self.sysAnnotations = sysAnnotations
        self.semantic = semantic
        self.token = token

        # Validate the name
        if self.token.text == '':
            # Generate an anonymous name
            self.token.text = '@{0}'.format(id(self))

    def validate(self):
        """Validate the object."""
        raise DevError()

    def default_location(self, kind, templateParameters=None, parameters=None):
        """
        Return the default location within the library.
        
        Args:
            kind (objects.LocationKind): The location kind of the final RelativeLocation.
            templateParameters (list of objects.TemplateParameter): The template parameters at the final RelativeLocation.
            parameters (list of objects.Parameter): The parameters at the final RelativeLocation.
        Returns:
            objects.Location: A location within the library.
        """
        rl = [RelativeLocation(kind, self.token.text, templateParameters=templateParameters, parameters=parameters)]
        if (self.parent) and (self.parent.parent):
            return Location(self.parent.location().path + rl)
        else:
            return Location(rl)

    def validate_no_system_annotations(self):
        """Ensure that there are no system annotation associated to this object."""
        if self.sysAnnotations:
            raise UnsupportedSystemAnnotationError(self.class_name(), self.sysAnnotations[0])

    def validate_system_annotations(self, *compatibleNames):
        """
        Validate all system annotations.

        Args:
            compatibleNames (list of str): A list, containing the compatible annotation names.
        """
        for annotation in self.sysAnnotations:
            if annotation.token.text not in compatibleNames:
                raise UnsupportedSystemAnnotationError(self.class_name(), annotation)

class Reference(Named):
    """An external library reference."""

    @Decorators.validated
    def __init__(self, token, userAnnotations, sysAnnotations, semantic):
        """
        Initialize the object.

        Args:
            token (lexer.Symto): The reference text.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
        """
        super().__init__(None, token, userAnnotations, sysAnnotations, semantic)

    def validate(self):
        """Validate the object."""
        self.validate_no_system_annotations()

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        return self.default_location(LocationKind.Reference)

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return self.token.text

class Namespace(Named):
    """
    A namespace within a library.
    
    Attributes:
        objects (list of objects.Named): The Named object instances within the namespace.
    """

    @Decorators.validated
    def __init__(self, parent, token, userAnnotations, sysAnnotations, semantic):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
        """
        Named.__init__(self, parent, token, userAnnotations, sysAnnotations, semantic)
        self.objects = []

    def validate(self):
        """Validate the object."""
        self.validate_system_annotations('private', 'deprecate')

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        return self.default_location(LocationKind.Namespace)

    @staticmethod
    def parse(parser, args):
        """
        Parse a namespace.

        Args:
            parser (parsers.UnitParser): The parser to use.
            args: Unused.
        Returns:
            objects.Namespace: The namespace object or None, if no namespace was parsed.
        """
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        if not parser.match('namespace'):
            return None

        # Name token
        token = parser.match_kind_optional(Token.Name, Token.Text, '')
        
        # Semantic
        semantic = Annotation.parse_semantic(parser)
        
        parser.expect('{')

        # Grab the current parent
        parent = parser.namespaceStack[-1]
        
        # Create the namespace object
        namespace = Namespace(parent, token, userAnnotations, sysAnnotations, semantic)
        
        # Register it with the parent
        parent.objects.append(namespace)

        # Set it as the active namespace
        parser.namespaceStack.append(namespace)
        
        # Parse all objects within it
        parser.gather_namespace_objects()
        parser.expect('}')

        # Restore the parent namespace
        parser.namespaceStack.pop()

        return namespace

class TemplateObject(Named):
    """
    A templatable object.
    
    Attributes:
        body (list of lexer.Symto): The template body, represented by a list of tokens.
    """

    def __init__(self, parent, token, userAnnotations, sysAnnotations, semantic, body):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
            body (list of lexer.Symto): The template body, represented by a list of tokens.
        """
        super().__init__(parent, token, userAnnotations, sysAnnotations, semantic)
        self.body = body

class InstructionKind(Enum):
    """
    Enumeration of possible Instruction kinds.
    
    Attributes:
        Expression (int): The Instruction is an Expression.
        Break (int): The Instruction is a break statement.
        Return (int): The Instruction is a return statement.
        Continue (int): The Instruction is a continue statement.
        If (int): The Instruction is a branch statement.
        For (int): The Instruction is a for-loop.
        While (int): The Instruction is a while-loop.
        Elif (int): The Instruction is an else-if branch.
        Else (int): The Instruction is an else branch.
    """

    Expression = 0
    # Jmps
    Break = 1
    Return = 2
    Continue = 3
    # If has to be first here
    If = 4
    For = 5
    While = 6
    Do = 7
    Elif = 8
    Else = 9

class Instruction(Named):
    """
    An instruction within a function.
    
    Attributes:
        kind (objects.InstructionKind): The instruction type specifier.
        expression (objects.Expression): The expression within the instruction.
        instructions (list of objects.Instruction): The sub-instructions within the instruction (e.g. a for-loop body).
        forInits (list of objects.Expression): The expressions for the for-loop initialization.
        forPredicates (list of objects.Expression): The expressions for the for-loop predicates.
        forSteps (list of objects.Expression): The expressions for the for-loop step.
    """

    @Decorators.validated
    def __init__(self, parent, token, userAnnotations, sysAnnotations, semantic, kind, *, expression=None, instructions=None, forInits=None, forPredicates=None, forSteps=None):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): An anomymous identifier for the object.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
            kind (objects.InstructionKind): The instruction type specifier.
            expression (objects.Expression): The expression within the instruction.
            instructions (list of objects.Instruction): The sub-instructions within the instruction (e.g. a for-loop body).
            forInits (list of objects.Expression): The expressions for the for-loop initialization.
            forPredicates (list of objects.Expression): The expressions for the for-loop predicates.
            forSteps (list of objects.Expression): The expressions for the for-loop step.
        """
        super().__init__(parent, token, userAnnotations, sysAnnotations, semantic)
        self.kind = kind
        self.expression = expression
        self.instructions = instructions
        self.forInits = forInits
        self.forPredicates = forPredicates
        self.forSteps = forSteps

    def validate(self):
        """Validate the object."""
        self.validate_no_system_annotations()

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        return self.default_location(LocationKind.Instruction)

    @staticmethod
    def expect_expression(parser, endDelim):
        """
        Parse the next expression.
        
        Args:
            parser (parsers.UnitParser): The parser to use.
            endDelim (str): The end delimiter.
        Returns:
            objects.Expression: The expression.
        """
        # Parse the next expression with endDelim being the end delimiter
        expression = Expression.parse(parser, [endDelim])

        # Throw an error if no expression was parsed or if it is the empty expression
        if (expression is None) or (not expression.postfixAtoms):
            raise MissingExpressionError(parser.token.anchor)
        
        # Consume the end delimiter
        parser.consume()

        return expression

    @staticmethod
    def parse_parenthesized_expression(parser):
        """
        Parse the next parenthesized expression.
        
        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            objects.Expression, objects.Annotation: The expression and semantic of the instruction.
        """
        parser.expect('(')
        
        # Require a non-empty expression
        expression = Instruction.expect_expression(parser, ')')

        # Parse the semantic after if(expression) : SEMANTIC
        semantic = Annotation.parse_semantic(parser)

        return expression, semantic

    @staticmethod
    def parse_instruction_body(parser):
        """
        Parse all instructions in the body.
        
        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            list of objects.Instruction: The list of instructions.
        """
        parser.expect('{')
        # Parse all instructions in the body until } is found
        instructions = parser.gather_objects([Instruction], args=['}'])
        parser.expect('}')
        return instructions

    @staticmethod
    def parse(parser, args):
        """
        Parse an instruction.

        Args:
            parser (parsers.UnitParser): The parser to use.
            args (list of str): The end delimiter list.
        Returns:
            objects.Instruction: The instruction object or None, if no instruction was parsed.
        """
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        # Create a dummy token (anonymous) for the instruction
        token = Symto.from_token(parser.token, Token.Text, '')

        # Grab the current parent
        parent = parser.namespaceStack[-1]
        
        # Control flow statements
        # Convert the keywords to lower-case representation and pair with its kind
        ikMap = [(e.name.lower(), e) for e in InstructionKind][InstructionKind.If.value:]

        # Use the map to associate (if, InstructionKind.If) to the next match
        # Returns InstructionKind.Expression if none of the instruction kinds was matched
        kind = parser.match_map(ikMap, InstructionKind.Expression)

        # Should be self-explanatory
        if kind != InstructionKind.Expression:
            if kind not in [InstructionKind.Do, InstructionKind.For, InstructionKind.Else]:
                # KEYWORD(EXPRESSION)
                expression, semantic = Instruction.parse_parenthesized_expression(parser)
                instructions = Instruction.parse_instruction_body(parser)
                return Instruction(parent, token, userAnnotations, sysAnnotations, semantic, kind, expression=expression, instructions=instructions)
            elif kind == InstructionKind.Do:
                # DO { ... } WHILE(EXPRESSION)
                instructions = Instruction.parse_instruction_body(parser)
                parser.expect('while')
                expression, semantic = Instruction.parse_parenthesized_expression(parser)
                return Instruction(parent, token, userAnnotations, sysAnnotations, semantic, kind, expression=expression, instructions=instructions)
            elif kind == InstructionKind.For:
                # FOR(INIT_EXPR, INIT_EXPR; COND, COND; STEP, STEP)
                parser.expect('(')
                forInits = parser.gather_objects([Expression], ',', args=[',', ';'])
                parser.expect(';')
                forPredicates = parser.gather_objects([Expression], ',', args=[',', ';'])
                parser.expect(';')
                forSteps = parser.gather_objects([Expression], ',', args=[',', ')'])
                parser.expect(')')
                semantic = Annotation.parse_semantic(parser)
                instructions = Instruction.parse_instruction_body(parser)
                return Instruction(parent, token, userAnnotations, sysAnnotations, semantic, kind, instructions=instructions, forInits=forInit, forPredicates=forPredicates, forSteps=forSteps)
            elif kind == InstructionKind.Else:
                # ELSE { ... }
                semantic = Annotation.parse_semantic(parser)
                instructions = Instruction.parse_instruction_body(parser)
                return Instruction(parent, token, userAnnotations, sysAnnotations, semantic, kind, instructions=instructions)
            else:
                raise DevError()
        else:
            # Early-out if the current token is one of the end delimiters
            if parser.token.text in args:
                return None

            # Look for jumps (same as above)
            ikMap = [(e.name.lower(), e) for e in InstructionKind][InstructionKind.Break.value:InstructionKind.If.value]
            kind = parser.match_map(ikMap, InstructionKind.Expression)

            # Assume there is no expression (e.g. break)
            expression = None
            if kind in [InstructionKind.Expression, InstructionKind.Return]:
                expression = parser.try_parse_any([Expression], [';', '{']) # Disambiguation with nested functions
                if expression is None:
                    return None

                # Forward annotations (requires re-validation)
                expression.userAnnotations = userAnnotations
                expression.sysAnnotations = sysAnnotations
                expression.validate()

            if parser.match('{'): # Disambiguation with nested functions
                return None

            parser.expect(';')

            return Instruction(parent, token, [], [], None, kind, expression=expression) 

class ExpressionAtomKind(Enum):
    """
    Enumeration of all ExpressionAtom kinds.

    Attributes:
        Var (int): The ExpressionAtom is a variable.
        Number (int): The ExpressionAtom is a number.
        FunctionBegin (int): The ExpressionAtom is the start of a function invocation.
        FunctionEnd (int): The ExpressionAtom is the end of a function invocation.
        ArrayBegin (int): The ExpressionAtom is the start of an array.
        ArrayEnd (int): The ExpressionAtom is the end of an array.
        TemplateBegin (int): The ExpressionAtom is the start of a template.
        TemplateEnd (int): The ExpressionAtom is the end of a template.
        UnaryOp (int): The ExpressionAtom is a unary operator.
        BinaryOp (int): The ExpressionAtom is a binary operator.
        Delimiter (int): The ExpressionAtom is a delimiter.
    """

    Var = 0
    Number = 1
    FunctionBegin = 2
    FunctionEnd = 3
    ArrayBegin = 4
    ArrayEnd = 5
    TemplateBegin = 6
    TemplateEnd = 7
    UnaryOp = 8
    BinaryOp = 9
    Delimiter = 10

class ExpressionAtom:
    """
    An atom within an Expression.
    
    Attributes:
        token (lexer.Symto): The atom token.
        kind (objects.ExpressionAtomKind): The atom kind.
    """

    def __init__(self, token, kind):
        """
        Initialize the object.

        Args:
            token (lexer.Symto): The atom token.
            kind (objects.ExpressionAtomKind): The atom kind.
        """
        self.token = token
        self.kind = kind

class ExpressionAST:
    """
    An AST within an Expression.
    
    Args:
        atom (objects.ExpressionAtom): The expression atom.
        parent (objects.ExpressionAST): The parent expression AST.
        children (list of objects.ExpressionAST): The child expression ASTs.
    """

    def __init__(self, atom, parent, children=None):
        """
        Initialize the object.

        Args:
            atom (objects.ExpressionAtom): The expression atom.
            parent (objects.ExpressionAST): The parent expression AST.
            children (list of objects.ExpressionAST): The child expression ASTs.
        """
        self.atom = atom
        self.parent = parent
        self.children = [] if children is None else children

class Expression(Named):
    """
    An expression.
    
    Args:
        tokens (list of lexer.Symto): The expression token list.
        postfixAtoms (list of objects.ExpressionAtom): The expression atoms.
        ast (objects.ExpressionAST): The expression AST.
    """

    @Decorators.validated
    def __init__(self, parent, userAnnotations, sysAnnotations, tokens):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            tokens (list of lexer.Symto): The expression token list.
        """
        token = Symto.from_token(tokens[0], Token.Text, '')
        super().__init__(parent, token, userAnnotations, sysAnnotations, None)
        self.tokens = tokens
        self.postfixAtoms = Expression.to_postfix(tokens) # Convert to RPN
        self.ast = Expression.to_ast(self.postfixAtoms) # RPN to AST

    def validate(self):
        """Validate the object."""
        self.validate_no_system_annotations()

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        return Location([RelativeLocation(LocationKind.Unresolved, self.token.text)])

    @staticmethod
    def to_postfix(tokens):
        """
        Convert a token list to a postfix expression.

        Args:
            tokens (list of lexer.Symto): The token list.
        Returns:
            list of objects.ExpressionAtom: The expression atoms, in RPN.
        """
        class State(Enum):
            Default = 0
            Function = 1
            Array = 2
            Template = 3
            Tuple = 4

        out, stack, states = [], [], [State.Default]
        isNextOpenParenFunction = False
        wasLastTerminal = False
        # K=2 lookahead
        for i, t in enumerate(tokens):
            prev = tokens[i-1] if i > 0 else None
            t1 = tokens[i+1] if i < len(tokens) - 1 else None
            t2 = tokens[i+2] if i < len(tokens) - 2 else None

            if t.isTerminal:
                if wasLastTerminal:
                    raise InvalidExpressionError(t.anchor)

                # Set state for next cycle
                wasLastTerminal = True

                kind = ExpressionAtomKind.Number if t.isNumber else ExpressionAtomKind.Var
                # K=1 lookahead
                if t1 is not None:
                    # Function, Template
                    if t.kind == Token.Name:
                        if t1.text == '(':
                            kind = ExpressionAtomKind.FunctionBegin
                            states.append(State.Function)
                            stack.append(ExpressionAtom(t, ExpressionAtomKind.FunctionEnd))
                            isNextOpenParenFunction = True
                        elif t1.text == '[':
                            kind = ExpressionAtomKind.ArrayBegin
                            states.append(State.Array)
                            stack.append(ExpressionAtom(t, ExpressionAtomKind.ArrayEnd))
                        elif t1.text == '<':
                            # K=2 lookahead
                            if (not t2 is None) and (t2.kind == Token.Literal.String):
                                kind = ExpressionAtomKind.TemplateBegin
                                states.append(State.Template)
                                stack.append(ExpressionAtom(t, ExpressionAtomKind.TemplateEnd))

                out.append(ExpressionAtom(t, kind))
            else:
                # We can fetch a literal in the next cycle again
                wasLastTerminal = False

                if t.text == ',':
                    # Tuples > 1 are not allowed
                    if states[-1] == State.Tuple:
                        raise InvalidExpressionError(t.anchor)

                    # Keep track of how many ops were added
                    if Algorithm.pop_while(stack, lambda atom: not atom.token.isOpenBracket, lambda atom: out.append(atom)):
                        raise MissingBracketsError(t.anchor)
                
                    # If there were no ops added, the comma might be invalid if this is not an unbounded array
                    if states[-1] != State.Array:
                        if out[-1].kind in [ExpressionAtomKind.Delimiter, ExpressionAtomKind.FunctionBegin, ExpressionAtomKind.TemplateBegin]:
                            raise InvalidExpressionError(t.anchor)

                    # Add comma as delimiter
                    out.append(ExpressionAtom(t, ExpressionAtomKind.Delimiter))
                elif (t.isOpenBracket and t.text != '<') or (t.text == '<' and states[-1] == State.Template):
                    if t.text == '(':
                        # Is this a potential tuple?
                        if not isNextOpenParenFunction:
                            states.append(State.Tuple)
                        else:
                            isNextOpenParenFunction = False
                    elif t.text == '[':
                        if states[-1] != State.Array:
                            raise MissingArrayTypeError(t.anchor)

                    stack.append(ExpressionAtom(t, -1))
                elif (t.isCloseBracket and t.text != '>') or (t.text == '>' and states[-1] == State.Template): # Special case for template >
                    # Keep track of how many parameters were added
                    if Algorithm.pop_while(stack, lambda atom: not atom.token.text == t.matchingOpenBracket, lambda atom: out.append(atom)):
                        raise MissingBracketsError(t.anchor)

                    # Pop open bracket and state
                    stack.pop()
                    states.pop()

                    if stack:
                        # Pop function, template
                        if stack[-1].kind in [ExpressionAtomKind.FunctionEnd, ExpressionAtomKind.ArrayEnd, ExpressionAtomKind.TemplateEnd]:
                            out.append(stack[-1])
                            stack.pop()
                elif t.kind is Operator:
                    # Assume this is a unary op
                    kind = ExpressionAtomKind.UnaryOp if (t.isUnaryOp and prev is None) or (prev is not None and prev.text != ')' and not prev.isTerminal) else ExpressionAtomKind.BinaryOp
                    o1 = t
                
                    # Binary operator
                    if kind == ExpressionAtomKind.BinaryOp:
                        Algorithm.pop_while(stack, lambda o2: (o2.token.isOp) and ((o1.isBinaryLeftAssociative and o1.binaryPrecedence > o2.token.binaryPrecedence) or (o1.isBinaryRightAssociative and o1.binaryPrecedence >= o2.token.binaryPrecedence)), lambda o2: out.append(o2))
                
                    stack.append(ExpressionAtom(o1, kind))
                else:
                    raise InvalidExpressionError(t.anchor)

        # Remaining tokens to output
        if not Algorithm.pop_while(stack, lambda atom: not atom.token.text == '(', lambda atom: out.append(atom)):
            raise MissingBracketsError(stack[-1].token.anchor)

        return out

    @staticmethod
    def to_ast(postfixAtoms):
        """
        Convert a postfix (RPN) expression to an AST.

        Args:
            postfixAtoms (list of objects.ExpressionAtom): The postfix atoms.
        Returns:
            objects.ExpressionAST: The AST.
        """
        argCount = 0
        argCountStack = []
        argStack = []
        parent = None

        for atom in postfixAtoms:
            root = ExpressionAST(atom, parent)

            if atom.kind in [ExpressionAtomKind.Var, ExpressionAtomKind.Number]:
                argCount += 1 if argCount == 0 else 0
                argStack.append(root)
            elif atom.kind in [ExpressionAtomKind.FunctionBegin, ExpressionAtomKind.ArrayBegin, ExpressionAtomKind.TemplateBegin]:
                argCount += 1 if argCount == 0 else 0
                argCountStack.append(argCount)
                argCount = 0
                argStack.append(root)
                parent = root
            elif atom.kind in [ExpressionAtomKind.FunctionEnd, ExpressionAtomKind.ArrayEnd, ExpressionAtomKind.TemplateEnd]:
                numArgs = argCount+1
                if len(argStack) < numArgs:
                    raise InvalidExpressionError(atom.token.anchor)

                args = list(argStack[-numArgs:])

                # Replace root (End) with Begin
                root = args[0] # Begin
                args = args[1:] # Pop Begin
                root.children = args
                argStack = argStack[:-numArgs]
                argStack.append(root)
                parent = root.parent

                # Restore argument count
                argCount = argCountStack.pop()
            elif atom.kind == ExpressionAtomKind.UnaryOp:
                if len(argStack) < 1:
                    raise InvalidExpressionError(atom.token.anchor)

                argCount += 1 if argCount == 0 else 0
                child = argStack[-1]
                argStack = argStack[:-1]
                root.children = [child]
                argStack.append(root)
                parent = root.parent
            elif atom.kind == ExpressionAtomKind.BinaryOp:
                if len(argStack) < 2:
                    raise InvalidExpressionError(atom.token.anchor)

                argCount += 1 if argCount == 0 else 0
                lhs = argStack[-2]
                rhs = argStack[-1]
                argStack = argStack[:-2]
                root.children = [lhs, rhs]
                argStack.append(root)
                parent = root.parent
            elif atom.kind == ExpressionAtomKind.Delimiter:
                argCount += 1 if argCount > 0 else 2
            else:
                raise DevError()

        # Empty expression
        if not argStack:
            return None

        # Invalid expression
        if len(argStack) > 1:
            raise InvalidExpressionError(argStack[0].atom.token.anchor)

        # Just right!
        return argStack[0]

    @staticmethod
    def parse(parser, args):
        """
        Parse an expression.

        Args:
            parser (parsers.UnitParser): The parser to use.
            args (list of str): The end delimiter list.
        Returns:
            objects.Expression: The expression or None, if no expression was parsed.
        """
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)
        tokens = parser.until_any(args)
        try:
            return Expression(parser.namespaceStack[-1], userAnnotations, sysAnnotations, tokens)
        except:
            return None

class FunctionKind(Enum):
    """
    Enumeration of possible Function kinds.
    
    Regular (int): The Function is a regular function.
    Operator (int): The Function is an operator.
    Extension (int): The Function is an extension.
    """

    Regular = 0
    Operator = 1
    Extension = 2

class Parameter(Named):
    """
    A parameter in a Function signature.
    
    Args:
        typename (objects.Typename): The typename.
        isRef (bool): True, if the parameter is a reference. Otherwise False.
    """

    @Decorators.validated
    def __init__(self, parent, token, userAnnotations, sysAnnotations, semantic, typename, isRef):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
            typename (objects.Typename): The typename.
            isRef (bool): True, if the parameter is a reference. Otherwise False.
        """
        super().__init__(parent, token, userAnnotations, sysAnnotations, semantic)
        self.typename = typename
        self.isRef = isRef

    def validate(self):
        """Validate the object."""
        self.validate_no_system_annotations()

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        return self.typename.location()

    def __eq__(self, other):
        """
        Compare for equality with another parameter.
        
        Args:
            other (objects.Parameter): The other parameter.
        Returns:
            bool: True, if the two parameters are equal. Otherwise False.
        """
        return (self.isRef == other.isRef) and (self.location() == other.location())

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return str(self.typename)

    @staticmethod
    def parse(parser, args):
        """
        Parse an expression.

        Args:
            parser (lexer.UnitParser): The parser to use.
            args: Unused.
        Returns:
            objects.Expression: The parameter or None, if no parameter was parsed.
        """
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        # Reference prefix
        isRef = parser.match('ref')

        # Typename
        typename = Typename.try_parse(parser)
        if typename is None:
            return None

        # Name
        token = parser.match_kind_optional(Token.Name, Token.Text, '')

        # Semantic
        semantic = Annotation.parse_semantic(parser)

        return Parameter(parser.namespaceStack[-1], token, userAnnotations, sysAnnotations, semantic, typename, isRef)

class Function(TemplateObject, Namespace):
    """
    A function.
    
    Attributes:
        kind (objects.FunctionKind): The function kind.
        returnTypename (objects.Typename): The return typename.
        extensionTypename (objects.Typename): The extension typename.
        parameters (list of objects.Parameter): The parameters.
    """

    @Decorators.validated
    def __init__(self, parent, token, userAnnotations, sysAnnotations, semantic, body, kind, returnTypename, extensionTypename, parameters):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
            body (list of lexer.Symto): The template body.
            kind (objects.FunctionKind): The function kind.
            returnTypename (objects.Typename): The return typename.
            extensionTypename (objects.Typename): The extension typename.
            parameters (list of objects.Parameter): The parameters.
        """
        # Has to be bound first for validation
        self.kind = kind
        self.returnTypename = returnTypename
        self.extensionTypename = extensionTypename
        self.parameters = parameters
        Namespace.__init__(self, parent, token, userAnnotations, sysAnnotations, semantic)
        TemplateObject.__init__(self, parent, token, userAnnotations, sysAnnotations, semantic, body)

    def validate(self):
        """Validate the object."""
        if self.kind == FunctionKind.Extension:
            self.validate_system_annotations('static', 'private', 'deprecate')
        else:
            self.validate_system_annotations('private', 'deprecate')

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        location = self.default_location(LocationKind.Function, parameters=self.parameters)
        return location

    @staticmethod
    def parse_template(parser, isTemplate):
        """
        Parse the template.

        Args:
            parser (parsers.UnitParser): The parser to use.
            isTemplate (bool): Indicates whether the object should be parsed as a template.
        Returns:
            objects.Function: The function or None, if no function was parsed.
        """
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        kind = FunctionKind.Regular
        name = Symto.from_token(parser.token, parser.token.kind, '')
        extensionTypename = None
        parameters = []
        returnTypename = Typename.try_parse(parser, allowPartialMask=True)
        hasExplicitReturnType = returnTypename is not None

        # If the next token is not an ID then returnTypename
        # is actually the function name / scope and the return type is implicit void
        if parser.token.kind != Token.Name:
            if hasExplicitReturnType:
                name = returnTypename.scope[-1]
            returnTypenameToken = Symto.from_token(parser.token, Token.Name, 'void')
            returnTypename = Typename([returnTypenameToken])
        
        if hasExplicitReturnType:
            # Extensions can be explicitly scoped
            extensionTypename = Typename.try_parse(parser, allowPartialMask=True)

            # None: implicit return value
            # 1: Regular function or operator
            # 2 or more: extension
            if extensionTypename is not None:
                if len(extensionTypename.scope) >= 2:
                    # There should be no array specifications in the typename
                    if any(dim for dim in extensionTypename.dims):
                        return None

                    # The last token should not have template args
                    if extensionTypename.templateParameters[-1]:
                        return None

                    # Looks like an extension
                    kind = FunctionKind.Extension
                    name = extensionTypename.scope[-1]
                    extensionTypename = Typename(extensionTypename.scope[:-1], templateParameters=extensionTypename.templateParameters[:-1], dims=extensionTypename.dims[:-1]) if extensionTypename else None
                else:
                    if len(extensionTypename.scope) == 1:
                        # Regular function or operator
                        name = extensionTypename.scope[0]

                    # Zero out the extension guess
                    extensionTypename = None

                    # Operators require 1 extra token
                    if name.text == 'operator':
                        kind = FunctionKind.Operator
                        name = parser.expect_kind(Token.Operator)


            if parser.match('('):
                parameters = parser.gather_objects([Parameter], ',')
                parser.expect(')')

        semantic = Annotation.parse_semantic(parser)

        # Register the function with the current namespace
        parent = parser.namespaceStack[-1]
        func = Function(parent, name, userAnnotations, sysAnnotations, semantic, None, kind, returnTypename, extensionTypename, parameters)
        parser.namespaceStack.append(func)
        try:
            if isTemplate:
                func.body = parser.fetch_block('{', '}')
                parser.match(';')
            else:
                parent.objects.append(func)
                if parser.match('{'):
                    func.objects = parser.gather_objects([Namespace, Struct, Alias, Template, Instruction, Function], args=['}'])
                    parser.expect('}')
                    parser.match(';')
                elif not parser.match(';'):
                    parent.objects.pop()
                    return None
        finally:
            parser.namespaceStack.pop()

        return func

    @staticmethod
    def parse(parser, args):
        """
        Parse an expression.

        Args:
            parser (parsers.UnitParser): The parser to use.
            args: Unused.
        Returns:
            objects.Expression: The parameter or None, if no parameter was parsed.
        """
        return Function.parse_template(parser, False)

    def generate_from_template(self, prettyString):
        """
        Generate a template string from the parsed object.

        Args:
            prettyString (formatter.PrettyString): The string to append the Function to.
        """
        # ReturnType
        prettyString += str(self.returnTypename)

        # Name
        if self.kind == FunctionKind.Extension:
            prettyString += self.extensionName
        elif self.kind == FunctionKind.Operator:
            prettyString += 'operator' + self.name
        else:
            prettyString += self.name

        # Parameter signature
        numParameters = len(self.parameters)
        if numParameters > 0:
            prettyString += '('
            prettyString += '\n'
            prettyString.indentLevel += 1
            if numParameters > 0:
                for i in range(0, numParameters):
                    p = self.parameters[i]
                    prettyString += Annotation.usrlist_to_str(p.userAnnotations)
                    prettyString += Annotation.syslist_to_str(p.sysAnnotations)
                    if p.isRef:
                        prettyString += 'ref'
                    prettyString += str(p.typename)
                    prettyString += p.token.text
                    if p.semantic is not None:
                        prettyString += ': ' + p.semantic.text
                    if i < numParameters - 1:
                        prettyString += ','
                    prettyString += '\n'
            prettyString.indentLevel -= 1
            prettyString += ')'

class Member(Named):
    """
    A member inside a Structure object.
    
    Attributes:
        typename (objects.Typename): The member typename.
    """

    @Decorators.validated
    def __init__(self, parent, token, userAnnotations, sysAnnotations, semantic, typename):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
            typename (objects.Typename): The member typename.
        """
        super().__init__(parent, token, userAnnotations, sysAnnotations, semantic)
        self.typename = typename

        # Make sure that the typename is not void
        for token in typename.scope:
            if token.text == 'void':
                raise InvalidTypenameError(token.anchor)

    def validate(self):
        """Validate the object."""
        self.validate_system_annotations('private', 'deprecate')

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        return self.typename.location()

    @staticmethod
    def parse(parser, args):
        """
        Parse an member.

        Args:
            parser (parsers.UnitParser): The parser to use.
            args: Unused.
        Returns:
            objects.Expression: The parameter or None, if no parameter was parsed.
        """
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)
        typename = Typename.try_parse()
        if typename is None:
            return None

        name = parser.match_kind(Token.Name)
        name = Symto.from_token(parser.token, Token.Text, '') if name is None else name

        semantic = Annotation.parse_semantic(parser)
        parser.match(';')

        return Member(parser.namespaceStack[-1], name, userAnnotations, sysAnnotations, semantic, typename)

class MemberList(Named):
    """
    A collection of Member objects that share the same Typename.
    
    Attributes:
        members (list of objects.Member): A list of members.
        typename (objects.Typename): The member typename.
    """

    @Decorators.validated
    def __init__(self, parent, token, userAnnotations, sysAnnotations, semantic, members, typename):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
            members (list of objects.Member): A list of members.
            typename (objects.Typename): The member typename.
        """
        super().__init__(parent, token, userAnnotations, sysAnnotations, semantic)
        self.members = members
        self.typename = typename

    def validate(self):
        """Validate the object."""
        self.validate_system_annotations('private', 'deprecate')

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        return self.typename.location()

    @staticmethod
    def parse(parser, args):
        """
        Parse a list of members.

        Args:
            parser (parsers.UnitParser): The parser to use.
            args: Unused.
        Returns:
            objects.Expression: The parameter or None, if no member was parsed.
        """
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)
        typename = Typename.try_parse(parser)
        if typename is None:
            return None

        names = [parser.match_kind(Token.Name)]
        while parser.match(','):
            name = parser.match_kind(Token.Name)
            if name is None:
                break
            names.append(name)

        semantic = Annotation.parse_semantic(parser)
        if not parser.match(';'):
            return None
        
        members = []
        for name in names:
            name = Symto.from_token(parser.token, Token.Text, '') if name is None else name
            members.append(Member(parser.namespaceStack[-1], name, userAnnotations, sysAnnotations, semantic, typename))
        
        return MemberList(parser.namespaceStack[-1], name, userAnnotations, sysAnnotations, semantic, members, typename)

class Struct(TemplateObject, Namespace):
    """A structure."""

    def __init__(self, parent, token, userAnnotations, sysAnnotations, semantic, body):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
            body (list of lexer.Symto): The template body.
        """
        Namespace.__init__(self, parent, token, userAnnotations, sysAnnotations, semantic)
        TemplateObject.__init__(self, parent, token, userAnnotations, sysAnnotations, semantic, body)

    def validate(self):
        """Validate the object."""
        self.validate_system_annotations('private', 'deprecate')

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        return self.default_location(LocationKind.Type)

    @staticmethod
    def parse_template(parser, isTemplate):
        """
        Parse a structure.

        Args:
            parser (parsers.UnitParser): The parser to use.
            isTemplate (bool): Indicates whether the object should be parsed as a template.
        Returns:
            objects.Expression: The structure or None, if no struct was parsed.
        """
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        if not parser.match('struct'):
            return None

        token = parser.match_kind_optional(Token.Name, Token.Text, '')
        semantic = Annotation.parse_semantic(parser)

        # Register the struct with the current namespace
        parent = parser.namespaceStack[-1]
        struct = Struct(parent, token, userAnnotations, sysAnnotations, semantic, None)
        parser.namespaceStack.append(struct)
        try:
            if isTemplate:
                struct.body = parser.fetch_block('{', '}')
                parser.match(';')
            else:
                parent.objects.append(struct)
                if parser.match('{'):
                    struct.objects = parser.gather_objects([Namespace, Struct, Alias, Template, MemberList, Function], args=['}'])
                    parser.expect('}')
                    parser.match(';')
                elif not parser.match(';'):
                    parent.objects.pop()
                    return None
        finally:
            parser.namespaceStack.pop()

        return struct

    @staticmethod
    def parse(parser, args):
        """
        Parse a structure.

        Args:
            parser (parsers.UnitParser): The parser to use.
            args: Unused.
        Returns:
            objects.Expression: The parameter or None, if no struct was parsed.
        """
        return Struct.parse_template(parser, False)

    def generate_from_template(self, prettyString):
        """
        Generate a template string from the parsed object.

        Args:
            prettyString (formatter.PrettyString): The string to append the Struct to.
        """
        prettyString += 'struct ' + self.token.text

class Alias(TemplateObject):
    """
    A type alias.
    
    Attributes:
        targetTypename (objects.Typename): The target typename.
    """

    @Decorators.validated
    def __init__(self, parent, token, userAnnotations, sysAnnotations, semantic, body, targetTypename):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
            body (list of lexer.Symto): The template body.
            targetTypename (objects.Typename): The target typename.
        """
        TemplateObject.__init__(self, parent, token, userAnnotations, sysAnnotations, semantic, body)
        self.targetTypename = targetTypename
    
    def validate(self):
        """Validate the object."""
        self.validate_system_annotations('private', 'deprecate')

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        return self.default_location(LocationKind.Type)

    @staticmethod
    def parse_template(parser, isTemplate):
        """
        Parse the template.

        Args:
            parser (parsers.UnitParser): The parser to use.
            isTemplate (bool): Indicates whether the object should be parsed as a template.
        Returns:
            objects.Alias: The alias or None, if no alias was parsed.
        """
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)
        if not parser.match('using'):
            return None

        token = parser.match_kind_optional(Token.Name, Token.Text, '')
        semantic = Annotation.parse_semantic(parser)
        targetTypename = None
        body = None
        if isTemplate:
            body = parser.fetch_block('{', '}')
        else:
            parser.expect('{')
            targetTypename = Typename.parse(parser)
            parser.expect('}')
        parser.match(';')

        # Register the struct with the current namespace
        parent = parser.namespaceStack[-1]
        alias = Alias(parent, token, userAnnotations, sysAnnotations, semantic, body, targetTypename)
        if not isTemplate:
            parent.objects.append(alias)

        return alias

    @staticmethod
    def parse(parser, args):
        """
        Parse a structure.

        Args:
            parser (parsers.UnitParser): The parser to use.
            args: Unused.
        Returns:
            objects.Alias: The parameter or None, if no alias was parsed.
        """
        return Alias.parse_template(parser, False)

    def generate_from_template(self, prettyString):
        """
        Generate a template string from the parsed object.

        Args:
            prettyString (PrettyString): The string to append the Alias to.
        """
        prettyString += 'using ' + self.token.text

class Annotation:
    """
    An object annotation.
    
    Attributes:
        token (lexer.Symto): The name token.
        args (list of str): The annotation arguments.
    """

    def __init__(self, token, args):
        """
        Initialize the object.

        Args:
            token (lexer.Symto): The name token.
            args (list of str): The annotation arguments.
        """
        self.token = token
        self.args = args

    @staticmethod
    def list_to_str(collection, open, close=None):
        """
        Convert an annotation list to a string list.
        
        Args:
            collection (list of objects.Annotation): The annotation list.
            open (str): The opening string.
            close (str): The closing string.
        Returns:
            list of str: The string list.
        """
        result = ''
        for annotation in collection:
            result += open + annotation.token.text
            if len(annotation.args) > 0:
                result += '(' + ', '.join([t.text for t in annotation.args]) + ')'
            if close is not None:
                result += close
            result += '\n'
        return result

    @staticmethod
    def usrlist_to_str(collection):
        """
        Convert a user annotation list to a string-list.

        Args:
            collection (list of str): The user annotation list.
        Returns:
            list of str: The string list.
        """
        return Annotation.list_to_str(collection, '[', ']')

    @staticmethod
    def syslist_to_str(collection):
        """
        Convert a system annotation list to a string-list.

        Args:
            collection (list of str): The system annotation list.
        Returns:
            list of str: The string list.
        """
        return Annotation.list_to_str(collection, '@')

    @staticmethod
    def parse_annotation_interior(parser):
        """
        Parse the interior of an annotation.

        Args:
            parser (UnitParser): The parser to use.
        Returns:
            objects.Annotation: The annotation.
        """
        token = parser.expect_kind(Token.Name)
        parameters = parser.fetch_block('(', ')')
        parameters = [p for p in parameters if p.text != ',']
        return Annotation(token, parameters)

    @staticmethod
    def try_parse_sys(parser):
        """
        Parse the next system annotation.
        
        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            objects.Annotation: The system annotation or None, if no user annotation was found.
        """
        return Annotation.parse_annotation_interior(parser) if parser.match('@') else None

    @staticmethod
    def try_parse_user(parser):
        """
        Parse the next user annotation.
        
        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            objects.Annotation: The user annotation or None, if no user annotation was found.
        """
        if parser.match('['):
            annotation = Annotation.parse_annotation_interior(parser)
            parser.match(']')
            return annotation
        else:
            return None

    @staticmethod
    def parse_semantic(parser):
        """
        Parse the next semantic.
        
        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            objects.Annotation: The semantic annotation or None, if no semantic was found.
        """
        return Annotation.parse_annotation_interior(parser) if parser.match(':') else None

    @staticmethod
    def parse_annotations(parser):
        """
        Parse the next user and system annotations.
        
        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            list of objects.Annotation, list of objects.Annotation: The user and system annotations.
        """
        userAnnotations = []
        sysAnnotations = []
        while True:
            annotation = Annotation.try_parse_user(parser)
            if annotation is None:
                annotation = Annotation.try_parse_sys(parser)
                if annotation is None: break

                # Make sure the system annotation exists
                if not annotation.token.text in Annotation.sys_annotations():
                    raise UnknownSystemAnnotationError(annotation.token.anchor, annotation.token.text)

                sysAnnotations.append(annotation)
            else:
                userAnnotations.append(annotation)
        return userAnnotations, sysAnnotations

    @staticmethod
    def sys_annotations():
        """
        Return a list of system annotation names.
        
        Returns:
            set of str: A set of annotation names. 
        """
        return { 'static', 'private', 'noconstructor', 'deprecate' }

    @staticmethod
    def has(name, collection):
        """
        Test whether a annotation collection contains an annotation with a given name.

        Args:
            name (str): The name to search for.
            collection (list of objects.Annotation): The annotation collection.
        Returns:
            bool: True, if an annotation with the specified name exists in the collection. Otherwise False.
        """
        return any(e.token.text == name for e in collection)

class Typename(Locatable):
    """
    A typename.
    
    Attributes:
        dims (list of int): The array dimensions.
        scope (list of lexer.Symto): The name scope.
        scopeStrings (list of str): The token scope list as a string list.
        templateParameters (list of list of lexer.Symto): A template parameter list for each entry in the scope.
    """

    @Decorators.validated
    def __init__(self, scope, *, templateParameters=None, dims=None):
        """
        Initialize the object.

        Args:
            scope (list of lexer.Symto): The scope token list.
            templateParameters (list of list of lexer.Symto): A template parameter list for each entry in the scope.
            dims (list of int): The array dimensions. A value of None inside the list denotes an unbounded array.
        """
        assert scope

        super().__init__(None, scope[-1].anchor)
        self.scope = scope
        
        # Overwrite the scope strings
        self.scopeStrings = [token.text for token in scope]

        # Generate args if necessary
        if templateParameters is None:
            templateParameters = [[] for _ in scope]

        # Generate args if necessary
        if dims is None:
            dims = []

        self.templateParameters = templateParameters
        self.dims = dims

    def validate(self):
        """Validate the object."""
        firstToken = self.scope[0]

        # Validate the first scope token
        if firstToken.text in Language.systemTypenames:
            # The scope has to be exactly 1
            if len(self.scope) > 1:
                raise InvalidTypenameError(firstToken.anchor)

            # Default typenames are never templated
            if self.templateParameters[0]:
                raise InvalidTypenameError(firstToken.anchor)

            # void does not have array bounds
            if firstToken.text == 'void':
                if len(self.dims) > 0:
                    raise InvalidTypenameError(firstToken.anchor)

        # Validate the entire scope
        if len(self.scope) > 0:
            for token in self.scope[1:]:
                if token.text in Language.systemTypenames:
                    raise InvalidTypenameError(token.anchor)

        # Should not be a keyword
        if any(token.text in Language.keywords for token in self.scope):
            raise InvalidTypenameError(self.scope[0].anchor)

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        return Location([RelativeLocation(LocationKind.Unresolved, s, templateParameters=self.templateParameters[i]) for i, s in enumerate(self.scopeStrings)])

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        strings = []
        for i in range(len(self.scope)):
            templateParameters = self.templateParameters[i]

            # Base
            token = self.scope[i]
            s = token.text

            # Template args
            if templateParameters:
                s += '<'
                s += ', '.join(parameter.text for parameter in templateParameters)
                s += '>'

            strings.append(s)
        
        # Array dims
        if self.dims:
            s += '['
            s += ', '.join(arg.text if arg is not None else ':' for arg in self.dims)
            s += ']'

        return '.'.join(strings)

    @staticmethod
    def parse_template_parameters(parser, allowPartialMask=False):
        """
        Parse the template parameters of the typename.

        Args:
            parser (parsers.UnitParser): The parser to use.
            allowPartialMask (bool): Specifies whether partial masks are allowed, containing strings and identifiers.
        Returns:
            list of objects.TemplateParameter: The template parameter list.
        """
        templateParameters = []
        parameterMask = [Token.Name, Token.Literal.String] if allowPartialMask else [Token.Literal.String]
        if parser.match('<'):
            while True:
                token = parser.match_any_kind(parameterMask)
                if token is None:
                    break
                templateParameters += [token]
                parser.match(',')
            parser.expect('>')
        return templateParameters

    @staticmethod
    def parse_array_dimensions(parser):
        """
        Parse the array dimensions.

        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            list of int: The array dimensions.
        """
        dims = []
        if parser.match('['):
            missingBounds = True
            while True:
                # Known bounds
                token = parser.match_kind(Token.Number.Integer)
                if token is not None:
                    dims += [token]
                    missingBounds = False
                else:
                    # Unknown bounds
                    if missingBounds:
                        dims += [None]
                        missingBounds = False

                    token = parser.token
                    if parser.match(','):
                        missingBounds = True
                    else:
                        break
            parser.expect(']')
        return dims

    @staticmethod
    def try_parse(parser, allowPartialMask=False):
        """
        Parse a typename.

        Args:
            parser (parsers.UnitParser): The parser to use.
            allowPartialMask (bool): Specifies whether partial masks are allowed, containing strings and identifiers.
        Returns:
            list of objects.Typename: The Typename or None, if no Typename was parsed.
        """
        # Explicit scoping
        token = parser.match_kind(Token.Name)
        if token is None:
            return None

        scope = [token]
        templateParams = [Typename.parse_template_parameters(parser, allowPartialMask)]
        while True:
            token = parser.token
            if parser.match('.'):
                scope += [parser.expect_kind(Token.Name)]
                templateParams += [Typename.parse_template_parameters(parser, allowPartialMask)]
            else:
                break

        dim = Typename.parse_array_dimensions(parser)
        return Typename(scope, templateParameters=templateParams, dims=dim)

    @staticmethod
    def parse(parser):
        """
        Parse a typename.

        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            objects.Typename: The typename.
        """
        typename = Typename.try_parse(parser)
        if typename is None:
            raise TypenameExpectedError(parser.token.anchor)
        return typename

class TemplateParameter(Named):
    """A template parameter."""

    def __init__(self, parent, token, userAnnotations, sysAnnotations, semantic):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
        """
        super().__init__(parent, token, userAnnotations, sysAnnotations, semantic)

    @staticmethod
    def parse(parser, args):
        """
        Parse a template parameter.

        Args:
            parser (parsers.UnitParser): The parser to use.
            args: Unused.
        Returns:
            objects.TemplateParameter: The template parameter.
        """
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        name = parser.match_kind(Token.Name)
        if name is None:
            return None

        semantic = Annotation.parse_semantic(parser)
        return TemplateParameter(parser.namespaceStack[-1], name, userAnnotations, sysAnnotations, semantic)

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return self.token.text

    def  __hash__(self):
        """
        Return a hash value for this object.

        Returns:
            int: The hash value.
        """
        return hash(self.__str__())

    def __eq__(self, other):
        """
        Compare for equality with another object.

        Args:
            other (objects.TemplateParameter): The other object.
        Returns:
            bool: True, if both template parameters are equal. Otherwise False.
        """
        return self.token.text == other.token.text

class Template(Named):
    """
    A template.

    Attributes:
        parameters (list of objects.TemplateParameter): The template parameter list.
        obj (objects.TemplateObject): The template object attached to this template.
        namespaceList (list of objects.Namespace): The namespace list to locate this template.
        references (list of objects.Reference): The references to use when instantiating this template.
    """

    @Decorators.validated
    def __init__(self, parent, userAnnotations, sysAnnotations, semantic, parameters, obj, namespaceList, references):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            userAnnotations (list of objects.Annotation): The user annotations.
            sysAnnotations (list of objects.Annotation): The system annotations.
            semantic (lexer.Symto): The semantic annotation.
            parameters (list of objects.TemplateParameter): The template parameter list.
            obj (objects.TemplateObject): The template object attached to this template.
            namespaceList (list of objects.Namespace): The namespace list to locate this template.
            references (list of objects.Reference): The references to use when instantiating this template.
        """
        super().__init__(parent, obj.token, userAnnotations, sysAnnotations, semantic)
        self.obj = obj
        self.parameters = parameters
        self.namespaceList = namespaceList
        self.namespaceList.pop(0)
        self.references = references

        # Make sure that the template parameters are unique
        if len(set(self.parameters)) != len(self.parameters):
            raise DuplicateTemplateParameterError(self.token.anchor)

    def validate(self):
        """Validate the object."""
        self.validate_system_annotations('private', 'deprecate')

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        # Return the location of the underlying object, but add the template parameters
        loc = deepcopy(self.obj.location())

        # Change the last relative location to a template
        lastRelLoc = loc[-1]
        lastRelLoc.kind = LocationKind.Template
        lastRelLoc.templateParameters = self.parameters

        return loc

    def generate_translation_unit(self):
        """
        Generate a translation unit from the template.
        
        Returns:
            formatter.PrettyString: The pretty-formatted string.
        """
        result = PrettyString()

        # Namespace
        for namespace in self.namespaceList:
            result += Annotation.usrlist_to_str(namespace.userAnnotations)
            result += Annotation.syslist_to_str(namespace.sysAnnotations)
            result += 'namespace {0}'.format(namespace.token.text)
            if namespace.semantic is not None:
                result += ': ' + namespace.semantic.text
            result += ' {\n'
            result.indentLevel += 1

        # Emit the annotations
        result += Annotation.usrlist_to_str(self.userAnnotations)
        result += Annotation.syslist_to_str(self.sysAnnotations)

        # Emit object header
        self.obj.generate_from_template(result)

        # Template body
        if self.obj.semantic is not None:
            result += ': ' + self.obj.semantic.text
        result += ' {'
        result += '\n'
        result.indentLevel += 1
        result.value += PrettyString.from_tokens(self.obj.body, self.obj.body[0].line) if len(self.obj.body) > 0 else 1
        result += '\n'
        result.indentLevel -= 1
        result += '}'
        result += '\n'

        # Close namespace
        for namespace in self.namespaceList:
            result.indentLevel -= 1
            result += "}\n"

        return result.value.strip()

    @staticmethod
    def parse(parser, args):
        """
        Parse an expression.

        Args:
            parser (parsers.UnitParser): The parser to use.
            args: Unused.
        Returns:
            objects.Expression: The expression or None, if no expression was parsed.
        """
        userAnnotations, sysAnnotations = Annotation.parse_annotations(parser)

        if not parser.match('template'):
            return None

        parameters = []
        if parser.match('<'):
            parameters = parser.gather_objects([TemplateParameter], ',')
            parser.expect('>')
        semantic = Annotation.parse_semantic(parser)

        # Deduce the template kind
        templateClasses = [Struct, Alias, Function]
        for c in templateClasses:
            parser.push_state()
            obj = c.parse_template(parser, True)
            if obj is not None:
                parser.remove_state()
                break
            parser.pop_state()

        if obj is None:
            return None

        # Register the template with the current namespace
        parent = parser.namespaceStack[-1]
        template = Template(parent, userAnnotations, sysAnnotations, semantic, parameters, obj, list(parser.namespaceStack), list(parser.references))
        parent.objects.append(template)

        return template