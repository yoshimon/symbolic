﻿"""
Contains all source code object types in symbolic.

The types in this package are used to convert the textual representation of the symbolic source code to an in-memory representation.
This is later passed onto the dependency solver to resolve library dependencies, see symbolic.dag.
"""

# Built-in
import io
from enum import Enum
from functools import wraps

# Library
from pygments.token import Token, Text, Operator, Name, String, Number, Punctuation, Error

# Project
from symbolic.exceptions import *
from symbolic.lexer import SymbolicLexer, Symto
from symbolic.formatter import PrettyString
from symbolic.algorithm import Algorithm
from symbolic.language import Language
from symbolic.base_parser import BaseParser

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
        templateParameters ([objects.TemplateParameter]): The template parameters.
        parameters ([objects.Parameter]): The signature.
        dims ([int]): The array dimensions. This is only meaningful on leaf location.
    """

    def __init__(self, kind, name, *, templateParameters=None, parameters=None, dims=None):
        """Initialize the object.
        Args:
            kind (objects.LocationKind): The location type specifier.
            name (str): The location name.
            templateParameters ([objects.TemplateParameter]): The template parameters.
            parameters ([objects.Parameter]): The signature.
            dims ([int]): The array dimensions. This is only meaningful on leaf location.
        """
        self.kind = kind
        self.name = str(name)
        self.templateParameters = templateParameters if templateParameters is not None else []
        self.parameters = parameters if parameters is not None else []
        self.dims = [] if dims is None else dims

    def is_plain(self):
        """
        Return whether the relative location is plain.
        
        A location is considered to be plain when it does not have any (template) parameters.

        Returns:
            bool: True, if the location is plain. Otherwise False.
        """
        return (not self.templateParameters) and (not self.parameters)

    def might_be_equal_to(self, other):
        """
        Return whether two relative locations could be potentially equal.

        This function compares the signatures of both relative locations without
        looking at the parameter types.

        Returns:
            bool: True, if the signatures match, excluding a test for type equality. Otherwise False.
        """
        return (
            (self.kind == other.kind or self.kind == LocationKind.Unresolved or other.kind == LocationKind.Unresolved) and
            self.name == other.name and
            len(self.parameters) == len(other.parameters) and
            all(p0.isRef == p1.isRef for p0, p1 in zip(self.parameters, other.parameters)) and
            len(self.dims) == len(other.dims) and
            self.dims == other.dims and
            Algorithm.zip_all(self.templateParameters, other.templateParameters,
                              lambda p0, p1: p0.partialMatch is None or p1.partialMatch is None or p0.partialMatch == p1.partialMatch)
        )

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation of the object.
        """
        templateStr = "<{0}>".format(Algorithm.join_comma(self.templateParameters)) if self.templateParameters else ""
        dimsStr = "[{0}]".format(Algorithm.join_comma(self.dims)) if self.dims else ""
        parameterStr = "({0})".format(Algorithm.join_comma(self.parameters)) if self.parameters else ""
        return self.name + templateStr + dimsStr + parameterStr

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
            (self.templateParameters == other.templateParameters) and \
            (self.dims == other.dims)

    def location(self):
        """
        Converts the relative location to a location.

        Returns:
            objects.Location: The location object.
        """
        return Location([self])

class Location:
    """
    An (absolute) location within a library.
    
    Attributes:
        path ([objects.RelativeLocation]): A list of relative location specifiers.
    """

    def __init__(self, path):
        """
        Initialize the object.

        Args:
            path ([objects.RelativeLocation]): A list of relative location specifiers.
        """
        self.path = path

    def base(self):
        """
        Return the base location without array bounds.

        Returns:
            objects.Typename: The base typename.
        """
        return Location([RelativeLocation(rl.kind, rl.name, templateParameters=rl.templateParameters, parameters=rl.parameters) for rl in self.path])

    def dims(self):
        """
        Return the array dimensions of the last relative location.

        Returns:
            list: The array dimensions.
        """
        return self[-1].dims

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        if self.path:
            lastRL = self.path[-1]
            kindStr = " as {0}".format(str(lastRL.kind)) if lastRL.kind != LocationKind.Unresolved else ""
        else:
            kindStr = ""

        return Algorithm.join_dot(self.path) + kindStr

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

    def __len__(self):
        """
        Return the number of the location path elements.

        Returns:
            int: The number of path elements.
        """
        return len(self.path)

class Locatable:
    """
    A locatable object within a library.

    Multiple Locatable objects represent a hierarchy. An Anchor is used
    to locate the object within a library.
    
    Attributes:
        references ([objects.Reference]): The references visible to this locatable.
        parent (objects.Locatable): The parent object.
        grandParent (objects.Locatable or None): The grand-parent object.
        grandParentWithoutRoot (objects.Locatable or None): The grand-parent object or None if the grandparent is the root.
        anchor (Token): The anchor in the source code.        
    """

    def __init__(self, references, parent, anchor):
        """
        Initialize the object.
        
        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            anchor (Token): The anchor in the source code.
        """
        self.references = references
        self.parent = parent
        self.grandParent = parent.parent if parent else None
        self.grandParentWithoutRoot = self.grandParent if (self.grandParent) and (self.grandParent.parent) else None
        self.anchor = anchor

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        # Force this to be implemented by all derived classes
        assert(False)

    def has_reference(self, name):
        """
        Return whether a reference with a given name exists.

        Args
            name (str): The reference name.
        Returns:
            bool: True, if the reference exists. Otherwise False.
        """
        return any(str(ref) == name for ref in self.references)

class Named(Locatable):
    """
    A named object within a library.
    
    Attributes:
        token (lexer.Symto): A token which holds the name.
        annotations ([objects.Annotation]): The system annotations.
        semantic (lexer.Symto): The semantic annotation.
    """

    def __init__(self, references, parent, token, annotations, semantic):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this object.
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            annotations ([objects.Annotation]): The user annotations.
            semantic (lexer.Symto): The semantic annotation.
        """
        super().__init__(references, parent, token.anchor)
        assert token is not None
        assert annotations is not None
        self.annotations = annotations
        self.semantic = semantic
        self.token = token

        # Validate the name
        if self.token == '':
            # Generate an anonymous name
            self.token.text = '@{0}'.format(id(self))
            self.token.kind = Token.Text

    def validate(self):
        """Validate the object."""
        assert(False)

    def default_location(self, kind, *, templateParameters=None, parameters=None):
        """
        Return the default location within the library.
        
        Args:
            kind (objects.LocationKind): The location kind of the final RelativeLocation.
            templateParameters ([objects.TemplateParameter]): The template parameters at the final RelativeLocation.
            parameters ([objects.Parameter]): The parameters at the final RelativeLocation.
        Returns:
            objects.Location: A location within the library.
        """
        rl = [RelativeLocation(kind, str(self.token), templateParameters=templateParameters, parameters=parameters)]
        if self.parent and self.parent.parent:
            return Location(self.parent.location().path + rl)
        else:
            return Location(rl)

    def validate_no_system_annotations(self):
        """Ensure that there are no system annotation associated to this object."""
        for annotation in self.annotations:
            if str(annotation) in Language.annotations:
                raise UnsupportedAnnotationError(annotation)

    def validate_system_annotations(self, *compatibleNames):
        """
        Validate all system annotations.

        Args:
            compatibleNames ([str]): A list, containing the compatible annotation names.
        """
        self.validate_external_system_annotations(self.annotations, compatibleNames)

    def validate_external_system_annotations(self, where, compatibleNames):
        """
        Validate all external system annotations.

        Args:
            where (list): The annotation collection.
            compatibleNames ([str]): A list, containing the compatible annotation names.
        """
        compatibleNamesSet = set(compatibleNames)
        assert(compatibleNamesSet <= Language.annotations)

        for annotation in where:
            s = str(annotation)
            if s in Language.annotations and s not in compatibleNamesSet:
                raise UnsupportedAnnotationError(annotation)

    def __str__(self):
        """
        Return the token string of the named object.

        Returns:
            str: The name of the object.
        """
        return str(self.token)

class Reference(Named):
    """An external library reference."""

    @Decorators.validated
    def __init__(self, token, annotations, semantic):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            token (lexer.Symto): The reference text.
            annotations ([objects.Annotation]): The annotations.
            semantic (lexer.Symto): The semantic annotation.
        """
        super().__init__([], None, token, annotations, semantic)

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
        return str(self.token)

    def __eq__(self, other):
        """
        Return whether two references point to the same library.
        
        Args:
            other (objects.Reference): The reference to compare with.
        Returns:
            bool: True, if both references point to the same library. Otherwise False.
        """
        return str(self) == str(other)

class Namespace(Named):
    """
    A namespace within a library.
    
    Attributes:
        locatables ([objects.Locatable]): The locatable object instances within the namespace.
    """

    @Decorators.validated
    def __init__(self, references, parent, token, annotations, semantic):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            annotations ([objects.Annotation]): The annotations.
            semantic (lexer.Symto): The semantic annotation.
        """
        Named.__init__(self, references, parent, token, annotations, semantic)
        self.locatables = []

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
        annotations = Annotation.parse_annotations(parser)

        if not parser.match('namespace'):
            return None

        # Name token
        token = parser.match_name()
        if token is None:
            return None
        
        # NOTE(gokhan.ozdogan): namespaces do not have semantics.
        # This is to allow merging to work.
        
        parser.expect('{')

        # Grab the current parent
        parent = parser.namespace()
        
        # Create the namespace object
        namespace = Namespace(parser.references, parent, token, annotations, None)
        
        # Register it with the parent
        parent.locatables.append(namespace)

        # Set it as the active namespace
        parser.namespaceStack.append(namespace)
        
        # Parse all objects within it
        parser.gather_namespace_objects()
        parser.expect('}')

        # Restore the parent namespace
        parser.namespaceStack.pop()

        return namespace

    def merge(self, other):
        """
        Merge another namespace into this namespace.

        Args:
            other (objects.Namespace): The namespace to merge with.
        """
        # Copy annotations.
        self.annotations += other.annotations

        # Re-parent children.
        for locatable in other.locatables:
            locatable.parent = self
        
        self.validate()

class TemplateObject(Named):
    """
    A templatable object.
    
    Attributes:
        body ([lexer.Symto]): The template body, represented by a list of tokens.
    """

    def __init__(self, references, parent, token, annotations, semantic, body):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            annotations ([objects.Annotation]): The annotations.
            semantic (lexer.Symto): The semantic annotation.
            body ([lexer.Symto]): The template body, represented by a list of tokens.
        """
        super().__init__(references, parent, token, annotations, semantic)
        self.body = body

    def template_instance_name(self):
        """
        Returns the name for the template instance of this object.

        Returns:
            str: The name for the template instance of this object.
        """
        return "___{0}_template_{1}".format(str(self.token), str(id(self)))

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
        instructions ([objects.Instruction]): The sub-instructions within the instruction (e.g. a for-loop body).
        forInits ([objects.Expression]): The expressions for the for-loop initialization.
        forPredicates ([objects.Expression]): The expressions for the for-loop predicates.
        forSteps ([objects.Expression]): The expressions for the for-loop step.
    """

    @Decorators.validated
    def __init__(self, references, parent, token, annotations, semantic, kind, *, expression=None, instructions=None, forInits=None, forPredicates=None, forSteps=None):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): An anomymous identifier for the object.
            annotations ([objects.Annotation]): The annotations.
            semantic (lexer.Symto): The semantic annotation.
            kind (objects.InstructionKind): The instruction type specifier.
            expression (objects.Expression): The expression within the instruction.
            instructions ([objects.Instruction]): The sub-instructions within the instruction (e.g. a for-loop body).
            forInits ([objects.Expression]): The expressions for the for-loop initialization.
            forPredicates ([objects.Expression]): The expressions for the for-loop predicates.
            forSteps ([objects.Expression]): The expressions for the for-loop step.
        """
        super().__init__(references, parent, token, annotations, semantic)
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
            [objects.Instruction]: The list of instructions.
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
            args ([str]): The end delimiter list.
        Returns:
            objects.Instruction: The instruction object or None, if no instruction was parsed.
        """
        annotations = Annotation.parse_annotations(parser)

        # Create a dummy token (anonymous) for the instruction
        token = Symto.from_token(parser.token, Token.Text, '')

        # Grab the current parent
        parent = parser.namespace()
        
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
                return Instruction(parser.references, parent, token, annotations, semantic, kind, expression=expression, instructions=instructions)
            elif kind == InstructionKind.Do:
                # DO { ... } WHILE(EXPRESSION)
                instructions = Instruction.parse_instruction_body(parser)
                parser.expect('while')
                expression, semantic = Instruction.parse_parenthesized_expression(parser)
                return Instruction(parser.references, parent, token, annotations, semantic, kind, expression=expression, instructions=instructions)
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
                return Instruction(parser.references, parent, token, annotations, semantic, kind, instructions=instructions, forInits=forInits, forPredicates=forPredicates, forSteps=forSteps)
            elif kind == InstructionKind.Else:
                # ELSE { ... }
                semantic = Annotation.parse_semantic(parser)
                instructions = Instruction.parse_instruction_body(parser)
                return Instruction(parser.references, parent, token, annotations, semantic, kind, instructions=instructions)
            else:
                assert(False)
        else:
            # Early-out if the current token is one of the end delimiters
            if str(parser.token) in args:
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
                expression.annotations = annotations
                expression.validate()

            if parser.match('{'): # Disambiguation with nested functions
                return None

            parser.expect(';')

            return Instruction(parser.references, parent, token, [], None, kind, expression=expression) 

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
        children ([objects.ExpressionAST]): The child expression ASTs.
        isRef (bool): True, if the value below this subtree is meant to be interpreted as a reference. Otherwise False.
    """

    def __init__(self, atom, parent, children=None):
        """
        Initialize the object.

        Args:
            atom (objects.ExpressionAtom): The expression atom.
            parent (objects.ExpressionAST): The parent expression AST.
            children ([objects.ExpressionAST]): The child expression ASTs.
        """
        self.atom = atom
        self.parent = parent
        self.children = [] if children is None else children
        self.isRef = False
        
    def is_lvalue(self):
        """
        Return whether the atom is an L-value.

        Returns:
            bool: True, if the atom is an L-value. Otherwise False.
        """
        kind = self.atom.kind
        if self.children and str(self.atom.token) == ".":
            # x.x
            kind = self.children[0].atom.kind
        
        return kind == ExpressionAtomKind.Var

class Expression(Named):
    """
    An expression.
    
    Args:
        tokens ([lexer.Symto]): The expression token list.
        postfixAtoms ([objects.ExpressionAtom]): The expression atoms.
        ast (objects.ExpressionAST): The expression AST.
    """

    @Decorators.validated
    def __init__(self, references, parent, annotations, tokens):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            annotations ([objects.Annotation]): The annotations.
            tokens ([lexer.Symto]): The expression token list.
        """
        token = Symto.from_token(tokens[0], Token.Text, '')
        super().__init__(references, parent, token, annotations, None)
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
        return Location([RelativeLocation(LocationKind.Unresolved, str(self.token))])

    @staticmethod
    def stringify_template_argument_block(parser, result, previousToken):
        """
        Stringify the template arguments for a given template argument block.

        Args:
            parser (lexer.BaseParser): The parser to use.
            result (list): A list of tokens to append to.
        Returns:
            bool: True, if the parsing succeeded. Otherwise False.
        """
        # Fetch all tokens in <...> if such a block follows now.
        templateArgs = parser.fetch_block("<", [">", ">>"])
        if not templateArgs:
            return True

        # Last template argument may not be empty <a,b,>
        if templateArgs[-1] == ",":
            return False

        # Opening bracket.
        result.append(Symto.after_token(previousToken, Token.Operator, "<"))

        # Parse the template argument list.
        # Concatenate all tokens to a space-separated string,
        # commas indicate a new template argument.
        currentArg = ""
        for arg in templateArgs:
            if arg.bracketLevel == 0 and arg == ",":
                result.append(Symto.after_token(result[-1], Token.String, '"' + currentArg + '"'))
                currentArg = ""
            else:
                currentArg += " " + arg.without_quotes() if currentArg else arg.without_quotes()

        if currentArg:
            result.append(Symto.after_token(result[-1], Token.String, '"' + currentArg + '"'))

        if parser.previous() == ">>":
            result.append(Symto.after_token(result[-1], Token.String, '">"'))

        # Closing bracket.
        result.append(Symto.after_token(result[-1], Token.Operator, ">"))

        return True

    @staticmethod
    def stringify_template_arguments(tokens):
        """
        Simplify the template arguments to strings in a given token list.

        Args:
            tokens (list): The token sequence.
        Returns:
            list: The new tokens after simplification.
        """
        result = []

        # Identify template tokens <
        parser = BaseParser(None, None, tokens)
        while not parser.is_eof():
            # Name<...> is what we are looking for.
            t = parser.consume()
            result.append(t)
            if t.kind == Token.Name and not parser.is_eof():
                if not Expression.stringify_template_argument_block(parser, result, result[-1]):
                    raise InvalidExpressionError(tokens[0].anchor)

        return result

    @staticmethod
    def parenthesize_ternary_operators(tokens):
        """
        Parenthesize the expression between ternary operators.

        Args:
            tokens ([lexer.Symto]): The token list.
        Returns:
            ([lexer.Symto]): The new token list.
        """
        result = []

        openCount = 0
        for t in tokens:
            if t == "?":
                result.append(t)
                result.append(Symto.after_token(result[-1], Token.Punctuation, "("))
                openCount += 1
            elif t == ":":
                result.append(Symto.after_token(result[-1], Token.Punctuation, ")"))
                result.append(t)
                openCount -= 1
            else:
                result.append(t)

        if openCount != 0:
            raise InvalidExpressionError(tokens[0].anchor)

        return result

    @staticmethod
    def to_postfix(tokens):
        """
        Convert a token list to a postfix expression.

        Args:
            tokens ([lexer.Symto]): The token list.
        Returns:
            [objects.ExpressionAtom]: The expression atoms, in RPN.
        """
        class State(Enum):
            Default = 0
            Function = 1
            Array = 2
            Template = 3
            Tuple = 4

        # Stringify template arguments.
        tokens = Expression.stringify_template_arguments(tokens)

        # Parenthesize ternary operator as binary operators.
        tokens = Expression.parenthesize_ternary_operators(tokens)

        skip = 0
        out, stack, states = [], [], [State.Default]
        isNextOpenParenFunction = False
        wasLastTerminal = False
        # K=2 lookahead
        for i, t in enumerate(tokens):
            if skip > 0:
                skip -= 1
                continue

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
                    isVar = True

                    # Function, Template
                    if t.kind == Token.Name:
                        if t1 == '(':
                            kind = ExpressionAtomKind.FunctionBegin
                            states.append(State.Function)
                            stack.append(ExpressionAtom(t, ExpressionAtomKind.FunctionEnd))
                            isNextOpenParenFunction = True
                        elif t1 == '[':
                            kind = ExpressionAtomKind.ArrayBegin
                            states.append(State.Array)
                            stack.append(ExpressionAtom(t, ExpressionAtomKind.ArrayEnd))
                        elif t1 == '<':
                            # K=2 lookahead
                            if (not t2 is None) and (t2.kind == Token.Literal.String or t2 == ">"):
                                kind = ExpressionAtomKind.TemplateBegin
                                states.append(State.Template)
                                stack.append(ExpressionAtom(t, ExpressionAtomKind.TemplateEnd))

                out.append(ExpressionAtom(t, kind))
            else:
                # We can fetch a literal in the next cycle again
                wasLastTerminal = False

                if t == ',':
                    # We better have some output already.
                    if len(out) == 0:
                        raise InvalidExpressionError(t.anchor)

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
                    elif out[-1].kind == ExpressionAtomKind.ArrayBegin:
                        out.append(ExpressionAtom(None, ExpressionAtomKind.Number))

                    # Add comma as delimiter
                    out.append(ExpressionAtom(t, ExpressionAtomKind.Delimiter))
                elif (t.isOpenBracket and t != '<') or (t == '<' and states[-1] == State.Template):
                    if t == '(':
                        # Is this a potential tuple?
                        if not isNextOpenParenFunction:
                            states.append(State.Tuple)
                        else:
                            isNextOpenParenFunction = False
                    elif t == '[':
                        if states[-1] != State.Array:
                            raise MissingArrayTypeError(t.anchor)

                    stack.append(ExpressionAtom(t, -1))
                elif (t.isCloseBracket and t != '>') or (t == '>' and states[-1] == State.Template): # Special case for template >
                    # Keep track of how many parameters were added
                    if Algorithm.pop_while(stack, lambda atom: atom.token is not None and not atom.token == t.matchingOpenBracket, lambda atom: out.append(atom)):
                        raise MissingBracketsError(t.anchor)

                    # Pop open bracket and state
                    if stack[-1].token is not None:
                        stack.pop()

                    states.pop()

                    if stack:
                        # Pop function, array, template
                        stackTop = stack[-1]
                        if stackTop.kind in [ExpressionAtomKind.FunctionEnd, ExpressionAtomKind.ArrayEnd, ExpressionAtomKind.TemplateEnd]:
                            if stackTop.kind == ExpressionAtomKind.ArrayEnd and out[-1].kind in [ExpressionAtomKind.ArrayBegin, ExpressionAtomKind.Delimiter]:
                                # Array dimension = infinity
                                out.append(ExpressionAtom(None, ExpressionAtomKind.Number))

                            out.append(stackTop)
                            stack.pop()

                            # Combine template <>() or <>[] with dummy None op.
                            if stackTop.token is None:
                                out.append(ExpressionAtom(None, ExpressionAtomKind.BinaryOp))

                            # If this is a template function call switch to that state.
                            if t1 == "(":
                                out.append(ExpressionAtom(None, ExpressionAtomKind.FunctionBegin))
                                states.append(State.Function)
                                stack.append(ExpressionAtom(None, ExpressionAtomKind.FunctionEnd))
                                isNextOpenParenFunction = True
                                skip = 1
                            elif t1 == "[":
                                out.append(ExpressionAtom(None, ExpressionAtomKind.ArrayBegin))
                                states.append(State.Array)
                                stack.append(ExpressionAtom(None, ExpressionAtomKind.ArrayEnd))
                                skip = 1
                elif t.kind is Operator:
                    # Assume this is a unary op
                    kind = ExpressionAtomKind.UnaryOp if (t.isUnaryOp and prev is None) or (prev is not None and str(prev) not in [")", "]"] and not prev.isTerminal) else ExpressionAtomKind.BinaryOp
                    o1 = t
                
                    # Binary operator
                    if kind == ExpressionAtomKind.BinaryOp:
                        Algorithm.pop_while(stack, lambda o2: (o2.token.isBinaryOp) and ((o1.isBinaryLeftAssociative and o1.binaryPrecedence > o2.token.binaryPrecedence) or (o1.isBinaryRightAssociative and o1.binaryPrecedence >= o2.token.binaryPrecedence)), lambda o2: out.append(o2))
                
                    stack.append(ExpressionAtom(o1, kind))
                else:
                    raise InvalidExpressionError(t.anchor)

        # Remaining tokens to output
        if not Algorithm.pop_while(stack, lambda atom: not atom.token == '(', lambda atom: out.append(atom)):
            raise MissingBracketsError(stack[-1].token.anchor)

        return out

    @staticmethod
    def to_ast(postfixAtoms):
        """
        Convert a postfix (RPN) expression to an AST.

        Args:
            postfixAtoms ([objects.ExpressionAtom]): The postfix atoms.
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
                numArgs = argCount + 1
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

                if not atom.token == "ref":
                    root.children = [child]
                else:
                    if parent is None or \
                       parent.atom.kind != ExpressionAtomKind.FunctionBegin or \
                       not child.is_lvalue():
                        raise InvalidExpressionError(atom.token.anchor)

                    root = child
                    root.parent = parent
                    root.isRef = True

                argStack.append(root)
                parent = root.parent
            elif atom.kind == ExpressionAtomKind.BinaryOp:
                if len(argStack) < 2:
                    raise InvalidExpressionError(atom.token.anchor)

                argCount += 1 if argCount == 0 else 0
                lhs = argStack[-2]
                rhs = argStack[-1]
                argStack = argStack[:-2]

                if atom.token is not None:
                    root.children = [lhs, rhs]
                else:
                    # Fake template <>None() or <>None[] binary operator None
                    root = lhs
                    root.parent = parent
                    root.children.append(rhs)

                argStack.append(root)
                parent = root.parent
            elif atom.kind == ExpressionAtomKind.Delimiter:
                argCount += 1 if argCount > 0 else 2
            else:
                assert(False)

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
            args ([str]): The end delimiter list.
        Returns:
            objects.Expression: The expression or None, if no expression was parsed.
        """
        annotations = Annotation.parse_annotations(parser)
        try:
            tokens = parser.until_any(args)
            return Expression(parser.references, parser.namespace(), annotations, tokens)
        except:
            return None

    def __str__(self):
        """
        Return the expression as a string.

        Returns:
            str: The expression string.
        """
        return "".join(str(t) for t in self.tokens)

class FunctionKind(Enum):
    """
    Enumeration of possible Function kinds.
    
    Regular (int): The Function is a regular function.
    Operator (int): The Function is an operator.
    """

    Regular = 0
    Operator = 1

class Parameter(Named):
    """
    A parameter in a Function signature.
    
    Args:
        typename (objects.Typename): The typename.
        isRef (bool): True, if the parameter is a reference. Otherwise False.
    """

    @Decorators.validated
    def __init__(self, parent, token, annotations, semantic, typename, isRef):
        """
        Initialize the object.

        Args:
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            annotations ([objects.Annotation]): The annotations.
            semantic (lexer.Symto): The semantic annotation.
            typename (objects.Typename): The typename.
            isRef (bool): True, if the parameter is a reference. Otherwise False.
        """
        super().__init__(typename.references, parent, token, annotations, semantic)
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
        return ("ref " if self.isRef else "") + str(self.typename)

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
        annotations = Annotation.parse_annotations(parser)

        # Reference prefix
        isRef = parser.match('ref')

        # Typename
        typename = Typename.try_parse(parser)
        if typename is None:
            return None

        # Name
        token = parser.match_name_optional()

        # Semantic
        semantic = Annotation.parse_semantic(parser)

        return Parameter(parser.namespace(), token, annotations, semantic, typename, isRef)

class FunctionReference(Named):
    """
    A function reference.

    Attributes:
        kind (objects.FunctionKind): The function kind.
        returnTypename (objects.Typename): The return typename.
        parameters ([objects.Parameter]): The parameters.
    """

    def __init__(self, references, parent, token, kind, parameters):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            kind (objects.FunctionKind): The function kind.
            parameters ([objects.Parameter]): The parameters.
        """
        self.kind = kind
        self.parameters = parameters
        Named.__init__(self, references, parent, token, [], None)

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        defaultLocation = self.default_location(LocationKind.Function, parameters=self.parameters)

        # The location of this is only depending on the reference name.
        # We don't know what the resolved parent is.
        return Location([defaultLocation[-1]])

class Function(TemplateObject, Namespace):
    """
    A function.
    
    Attributes:
        kind (objects.FunctionKind): The function kind.
        returnTypename (objects.Typename): The return typename.
        parameters ([objects.Parameter]): The parameters.
    """

    @Decorators.validated
    def __init__(self, references, parent, token, annotations, semantic, body, kind, returnTypename, parameters, returnTypenameTemplateTokens, parameterTemplateTokens):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            annotations ([objects.Annotation]): The annotations.
            semantic (lexer.Symto): The semantic annotation.
            body ([lexer.Symto]): The template body.
            kind (objects.FunctionKind): The function kind.
            returnTypename (objects.Typename): The return typename.
            parameters ([objects.Parameter]): The parameters.
            returnTypenameTemplateTokens ([lexer.Symto]): The return typename tokens in a template.
            parameterTemplateTokens ([lexer.Symto]): The parameter tokens in a template.
        """
        # Has to be bound first for validation
        self.kind = kind
        self.returnTypename = returnTypename
        self.parameters = parameters
        self.returnTypenameTemplateTokens = returnTypenameTemplateTokens
        self.parameterTemplateTokens = parameterTemplateTokens
        Namespace.__init__(self, references, parent, token, annotations, semantic)
        TemplateObject.__init__(self, references, parent, token, annotations, semantic, body)

    def validate(self):
        """Validate the object."""
        self.validate_system_annotations('private', 'deprecate')

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: A location within the library.
        """
        return self.default_location(LocationKind.Function, parameters=self.parameters)

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
        annotations = Annotation.parse_annotations(parser)

        kind = FunctionKind.Regular
        parent = parser.namespace()
        parameters = []
        parameterTemplateTokens = None
        returnTypename = None
        returnTypenameTemplateTokens = None

        # 1. Return type
        if isTemplate:
            # Fetch all tokens until we run into one of these tokens.
            returnTypenameTemplateTokens = parser.until_any(["(", ":", "{", "operator"])

            # We require a return type.
            if returnTypenameTemplateTokens:
                return None

            # Go back one step, we parsed the function name.
            if parser.previous() != "operator":
                returnTypenameTemplateTokens.pop()
                if returnTypenameTemplateTokens:
                    return None
                
                parser.back()
        else:
            returnTypename = Typename.try_parse(parser)
            if returnTypename is None:
                return None

        # 2. Name
        # Operators require 1 extra token
        if parser.match("operator"):
            kind = FunctionKind.Operator
            name = parser.expect_kind(Token.Operator)
        else:
            # Try fetching the name.
            # The function might be anonymous so this can fail.
            name = parser.match_kind(Token.Name)
            if name is None:
                return None

        # Verify we have a valid function name.
        if str(name) in Language.keywords:
            return None

        # 3. Parameters
        if isTemplate:
            # Fetch whatever is in the upcoming parameter list, if there is one.
            parameterTemplateTokens = parser.fetch_block("(", [")"])
        elif parser.match('('):
            parameters = parser.gather_objects([Parameter], ',')
            parser.expect(')')

        semantic = Annotation.parse_semantic(parser)

        # Register the function with the current namespace
        parent = parser.namespace()
        func = Function(parser.references, parent, name, annotations, semantic, None, kind, returnTypename, parameters, returnTypenameTemplateTokens, parameterTemplateTokens)
        parser.namespaceStack.append(func)
        try:
            if isTemplate:
                func.body = parser.fetch_block('{', ['}'])
                parser.match(';')
            else:
                parent.locatables.append(func)
                if parser.match('{'):
                    func.locatables = parser.gather_objects([Instruction], args=['}'])
                    parser.expect('}')
                    parser.match(';')
                elif not parser.match(';'):
                    parent.locatables.pop()
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
        # Emit return type.
        prettyString += PrettyString.from_tokens(self.returnTypenameTemplateTokens, self.returnTypenameTemplateTokens[0].anchor.line)

        # Emit name.
        prettyString += self.template_instance_name()

        # Emit parameter list.
        if self.parameterTemplateTokens:
            prettyString += "("
            prettyString += "\n"
            prettyString.indentLevel += 1
            prettyString += PrettyString.from_tokens(self.parameterTemplateTokens, self.parameterTemplateTokens[0].anchor.line)
            prettyString.indentLevel -= 1
            prettyString += ")"

class Member(Named):
    """
    A member inside a Structure object.
    
    Attributes:
        typename (objects.Typename): The member typename.
    """

    @Decorators.validated
    def __init__(self, references, parent, token, annotations, semantic, typename):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            annotations ([objects.Annotation]): The annotations.
            semantic (lexer.Symto): The semantic annotation.
            typename (objects.Typename): The member typename.
        """
        super().__init__(references, parent, token, annotations, semantic)
        self.typename = typename

    def validate(self):
        """Validate the object."""
        self.validate_system_annotations('private', 'deprecate', 'implicit')

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
        Parse a member.

        Args:
            parser (parsers.UnitParser): The parser to use.
            args: Unused.
        Returns:
            objects.Expression: The parameter or None, if no parameter was parsed.
        """
        annotations = Annotation.parse_annotations(parser)
        typename = Typename.try_parse(parser)
        if typename is None:
            return None

        name = parser.match_name()
        if name is None:
            return None

        semantic = Annotation.parse_semantic(parser)
        parser.match(';')

        return Member(parser.references, parser.namespace(), name, annotations, semantic, typename)

class MemberList(Named):
    """
    A collection of Member objects that share the same Typename.
    
    Attributes:
        members ([objects.Member]): A list of members.
        typename (objects.Typename): The member typename.
    """

    @Decorators.validated
    def __init__(self, references, parent, token, annotations, semantic, members, typename):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            annotations ([objects.Annotation]): The annotations.
            semantic (lexer.Symto): The semantic annotation.
            members ([objects.Member]): A list of members.
            typename (objects.Typename): The member typename.
        """
        super().__init__(references, parent, token, annotations, semantic)
        self.members = members
        self.typename = typename

    def validate(self):
        """Validate the object."""
        self.validate_system_annotations('private', 'deprecate', 'implicit')

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
        annotations = Annotation.parse_annotations(parser)
        typename = Typename.try_parse(parser)
        if typename is None:
            return None

        names = [parser.match_kind(Token.Name)]
        while parser.match(','):
            name = parser.match_kind(Token.Name)
            if name is None:
                break
            names.append(name)

        if parser.match("="):
            # Parse until we hit a statement end of a semantic.
            expression = Expression.parse(parser, [";", ":"])

            # Throw an error if no expression was parsed or if it is the empty expression
            if (expression is None) or (not expression.postfixAtoms):
                raise MissingExpressionError(parser.token.anchor)

        semantic = Annotation.parse_semantic(parser)
        if not parser.match(';'):
            return None
        
        members = []
        for name in names:
            name = Symto.from_token(parser.token, Token.Text, '') if name is None else name
            members.append(Member(parser.references, parser.namespace(), name, annotations, semantic, typename))
        
        memberList = MemberList(parser.references, parser.namespace(), name, annotations, semantic, members, typename)
        parent = parser.namespace()
        parent.locatables.append(memberList)
        return memberList

class Struct(TemplateObject, Namespace):
    """A structure."""

    def __init__(self, references, parent, token, annotations, semantic, body):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            annotations ([objects.Annotation]): The annotations.
            semantic (lexer.Symto): The semantic annotation.
            body ([lexer.Symto]): The template body.
            parentTypename (None of objects.Typename): The parent typename.
            parentUserAnnotations (None or [objects.Annotation]): The user annotations for the parent typename.
            parentSysAnnotations (None or [objects.Annotation]): The system annotations for the parent typename.
            parentSemantic (None or lexer.Symto): The semantic annotation for the parent typename.
        """
        Namespace.__init__(self, references, parent, token, annotations, semantic)
        TemplateObject.__init__(self, references, parent, token, annotations, semantic, body)
        self.locatables = []

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

    def member_typename(self, errorAnchor, memberName):
        """
        Return the typename for a member.

        Args:
            memberName (str): The member to search for.
        Returns:
            objects.Typename: The typename.
        """
        memberNameStr = str(memberName)

        for locatable in self.locatables:
            if isinstance(locatable, MemberList):
                memberList = locatable
                for member in memberList.members:
                    if member.token == memberNameStr:
                        return memberList.typename

        raise MemberNotFoundError(errorAnchor, memberName)

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
        annotations = Annotation.parse_annotations(parser)

        if not parser.match('struct'):
            return None

        token = parser.match_name()
        if token is None:
            return None

        semantic = Annotation.parse_semantic(parser)

        # Register the struct with the current namespace
        parent = parser.namespace()
        struct = Struct(parser.references, parent, token, annotations, semantic, None)
        parser.namespaceStack.append(struct)
        try:
            if isTemplate:
                struct.body = parser.fetch_block('{', ['}'])
                parser.match(';')
            else:
                parent.locatables.append(struct)

                if parser.match('{'):
                    parser.gather_objects([MemberList], args=['}'])
                    parser.expect('}')
                    parser.match(';')
                elif not parser.match(';'):
                    parent.locatables.pop()
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
            prettyString (formatter.PrettyString): The string to append the Function to.
        """
        prettyString += 'struct ' + self.template_instance_name()

class Alias(TemplateObject):
    """
    A type alias.
    
    Attributes:
        targetTypename (objects.Typename): The target typename.
    """

    @Decorators.validated
    def __init__(self, references, parent, token, annotations, semantic, body, targetTypename):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            annotations ([objects.Annotation]): The annotations.
            semantic (lexer.Symto): The semantic annotation.
            body ([lexer.Symto]): The template body.
            targetTypename (objects.Typename): The target typename.
        """
        TemplateObject.__init__(self, references, parent, token, annotations, semantic, body)
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
        annotations = Annotation.parse_annotations(parser)
        if not parser.match('using'):
            return None

        token = parser.match_name()
        if token is None:
            return None

        semantic = Annotation.parse_semantic(parser)
        targetTypename = None
        body = None
        if isTemplate:
            body = parser.fetch_block('{', ['}'])
        else:
            parser.expect('{')
            targetTypename = Typename.parse(parser)
            parser.expect('}')
        parser.match(';')

        # Register the struct with the current namespace
        parent = parser.namespace()
        alias = Alias(parser.references, parent, token, annotations, semantic, body, targetTypename)
        if not isTemplate:
            parent.locatables.append(alias)

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
            prettyString (formatter.PrettyString): The string to append the Function to.
        """
        prettyString += 'using ' + self.template_instance_name()

class Annotation:
    """
    An object annotation.
    
    Attributes:
        expression (objects.Expression): The annotation expression.
    """

    def __init__(self, expression):
        """
        Initialize the object.

        Args:
            expression (objects.Expression): The annotation expression.
        """
        self.expression = expression
        self.token = self.expression.tokens[0]

    @staticmethod
    def collection_to_str(collection):
        """
        Convert an annotation list to a string.
        
        Args:
            collection ([objects.Annotation]): The annotation list.
        Returns:
            str: The string.
        """
        return "\n".join("[" + str(annotation) + "]" for annotation in collection)

    @staticmethod
    def try_parse_ex(parser, openDelim, endDelims, consumeEnd):
        """
        Parse the interior of an annotation.

        Args:
            parser (UnitParser): The parser to use.
            open (str): The opening delimiter.
            endDelims ([str]): The closing delimiters.
            consumeEnd (bool): True, if the matched end delimiter should be consumed. Otherwise False. 
        Returns:
            objects.Annotation: The annotation.
        """
        if not parser.match(openDelim):
            return None

        # Parse the next expression with endDelim being the end delimiter
        expression = Expression.parse(parser, endDelims)

        # Throw an error if no expression was parsed or if it is the empty expression
        if (expression is None) or (not expression.postfixAtoms):
            raise MissingExpressionError(parser.token.anchor)
        
        # Consume the end delimiter
        if consumeEnd:
            parser.consume()

        return Annotation(expression)

    @staticmethod
    def try_parse(parser):
        """
        Parse the next user annotation.
        
        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            objects.Annotation: The user annotation or None, if no user annotation was found.
        """
        return Annotation.try_parse_ex(parser, "[", ["]"], True)

    @staticmethod
    def parse_semantic(parser):
        """
        Parse the next semantic.
        
        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            objects.Annotation: The semantic annotation or None, if no semantic was found.
        """
        # Stop if we encounter an argument delimiter or a template end.
        return Annotation.try_parse_ex(parser, ":", [",", ";", ">", "{", ")"], False)

    @staticmethod
    def parse_annotations(parser):
        """
        Parse the next user and system annotations.
        
        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            [objects.Annotation]: The annotations.
        """
        annotations = []
        while True:
            annotation = Annotation.try_parse(parser)
            if annotation is None:
                break

            annotations.append(annotation)
        return annotations

    @staticmethod
    def has(name, collection):
        """
        Test whether a annotation collection contains an annotation with a given name.

        Args:
            name (str): The name to search for.
            collection ([objects.Annotation]): The annotation collection.
        Returns:
            bool: True, if an annotation with the specified name exists in the collection. Otherwise False.
        """
        return any(len(e.expression.tokens) == 1 and e.token == name for e in collection)

    def __str__(self):
        """
        Return a string representation of the annotation.

        Returns:
            str: The annotation string.
        """
        return str(self.expression)

class Typename(Locatable):
    """
    A typename.
    
    Attributes:
        dims ([int]): The array dimensions.
        scope ([lexer.Symto]): The name scope.
        scopeStrings ([str]): The token scope list as a string list.
        templateParameters ([[lexer.Symto]]): A template parameter list for each entry in the scope.
    """

    @Decorators.validated
    def __init__(self, references, parent, scope, *, templateParameters=None, dims=None):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            scope ([lexer.Symto]): The scope token list.
            templateParameters ([[lexer.Symto]]): A template parameter list for each entry in the scope.
            dims ([int]): The array dimensions. A value of None inside the list denotes an unbounded array.
        """
        assert scope

        super().__init__(references, parent, scope[-1].anchor)
        self.scope = scope
        
        # Overwrite the scope strings
        self.scopeStrings = [str(token) for token in scope]

        # Generate args if necessary
        if templateParameters is None:
            templateParameters = [[] for _ in scope]

        self.templateParameters = templateParameters
        self.dims = [] if dims is None else dims

    def validate(self):
        """Validate the object."""
        # Should not be a keyword.
        if any(str(token) in Language.keywords for token in self.scope):
            raise InvalidTypenameError(self.scope[0].anchor)

    def location(self):
        """
        Return the location within the library.

        Returns:
            objects.Location: An unresolved location within the library pointed to by this typename.
        """
        loc = Location([RelativeLocation(LocationKind.Unresolved, s, templateParameters=self.templateParameters[i]) for i, s in enumerate(self.scopeStrings)])
        loc[-1].dims = self.dims
        return loc

    def __eq__(self, other):
        """
        Return whether two typename objects are equal.

        Args:
            other (objects.Typename): The other typename object.
        Returns:
            bool: True, if the two objects are equal. Otherwise False.
        """
        return str(self) == str(other)

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
            s = str(token)

            # Template args
            if templateParameters:
                s += '<'
                s += ', '.join(str(parameter) for parameter in templateParameters)
                s += '>'

            # Array dims
            if self.dims:
                s += '['
                s += ', '.join(str(arg) if arg is not None else "" for arg in self.dims)
                s += ']'

            strings.append(s)

        return '.'.join(strings)

    @staticmethod
    def try_parse_template_parameters(parser, name):
        """
        Parse the template parameters of the typename.

        Args:
            parser (parsers.UnitParser): The parser to use.
            name (lexer.Symto): The typename token.
        Returns:
            [objects.TemplateParameter]: The template parameter list.
        """
        result = []

        parser.push_state()
        if Expression.stringify_template_argument_block(parser, result, name):
            # Get rid of opening and closing brackets.
            result = result[1:-1]

            # Convert to template parameters.
            result = [TemplateParameter(parser.references, None, t, [], None, t) for t in result]
        else:
            parser.pop_state()

        return result

    @staticmethod
    def try_parse_array_dimensions(parser):
        """
        Parse the array dimensions.

        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            [int] or None: The array dimensions.
        """
        dims = []
        
        if not parser.match("["):
            return dims

        missingBounds = True
        while True:
            # Known bounds
            token = parser.match_kind(Token.Number.Integer)
            if token is not None:
                dim = int(str(token))
                if dim < Language.minArrayDim:
                    raise InvalidArrayDimensionsError(token.anchor)

                dims.append(dim)
                missingBounds = False
            else:
                # Unknown bounds
                if missingBounds:
                    dims.append(None)
                    missingBounds = False

                token = parser.token
                if parser.match(','):
                    missingBounds = True
                else:
                    break

        if not parser.match("]"):
            return None

        return dims

    @staticmethod
    def try_parse(parser):
        """
        Parse a typename.

        Args:
            parser (parsers.UnitParser): The parser to use.
        Returns:
            [objects.Typename]: The Typename or None, if no Typename was parsed.
        """
        # Explicit scoping
        token = parser.match_kind(Token.Name)
        if token is None:
            return None

        scope = [token]
        templateParams = [Typename.try_parse_template_parameters(parser, token)]
        while True:
            token = parser.token
            if parser.match('.'):
                scope += [parser.expect_kind(Token.Name)]
                templateParams += [Typename.try_parse_template_parameters(parser, token)]
            else:
                break

        dim = Typename.try_parse_array_dimensions(parser)
        if dim is None:
            return None

        parent = parser.namespace()
        return Typename(parser.references, parent, scope, templateParameters=templateParams, dims=dim)

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

    @staticmethod
    def from_location(references, location):
        """
        Create a typename from a location.
        
        Args:
            references ([objects.Reference]): The references at the specified location.
            location (objects.Location): The location.
        Returns:
            objects.Typename: The corresponding typename.
        """
        scope = [Symto(Token.String, "", "", rl.name, 0, 0) for rl in location]
        templateParams = [[Symto(Token.String, "", "", p.name, 0, 0) for p in rl.templateParameters] for rl in location]
        return Typename(references, None, scope, templateParameters=templateParams, dims=location[-1].dims)

class TemplateParameter(Named):
    """A template parameter."""

    def __init__(self, references, parent, token, annotations, semantic, partialMatch):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            token (lexer.Symto): A token which holds the name.
            annotations ([objects.Annotation]): The annotations.
            semantic (lexer.Symto): The semantic annotation.
            partialMatch (lexer.Symto or None): A token that represents the partial template mask.
        """
        super().__init__(references, parent, token, annotations, semantic)
        self.partialMatch = partialMatch

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
        annotations = Annotation.parse_annotations(parser)

        # Name
        name = parser.match_kind(Token.Name)
        if name is None:
            return None

        # Partial template specialization using = VALUE syntax
        partialMatch = parser.expect_kind(Token.String) if parser.match("=") else None

        semantic = Annotation.parse_semantic(parser)
        return TemplateParameter(parser.references, parser.namespace(), name, annotations, semantic, partialMatch)

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return str(self.token)

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
        return self.token == other.token

class Template(Named):
    """
    A template.

    Attributes:
        parameters ([objects.TemplateParameter]): The template parameter list.
        obj (objects.TemplateObject): The template object attached to this template.
    """

    @Decorators.validated
    def __init__(self, references, parent, annotations, semantic, parameters, obj):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references visible to this locatable.
            parent (objects.Locatable): The parent object.
            annotations ([objects.Annotation]): The annotations.
            semantic (lexer.Symto): The semantic annotation.
            parameters ([objects.TemplateParameter]): The template parameter list.
            obj (objects.TemplateObject): The template object attached to this template.
        """
        super().__init__(references, parent, obj.token, annotations, semantic)
        self.obj = obj
        self.parameters = parameters

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
        loc = Location(self.obj.location().path)

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

        # Emit the references.
        result += "\n".join("import {0};".format(str(ref)) for ref in self.references)

        # Emit the annotations
        result += Annotation.collection_to_str(self.annotations)

        # Emit object header
        self.obj.generate_from_template(result)

        # Template body
        if self.obj.semantic is not None:
            result += ': ' + str(self.obj.semantic)
        result += ' {'
        result += '\n'
        result.indentLevel += 1

        if len(self.obj.body) > 0:
            result.value += PrettyString.from_tokens(self.obj.body, self.obj.body[0].anchor.line)

        result += '\n'
        result.indentLevel -= 1
        result += '}'
        result += '\n'

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
        annotations = Annotation.parse_annotations(parser)

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
        parent = parser.namespace()
        template = Template(parser.references, parent, annotations, semantic, parameters, obj)
        parent.locatables.append(template)

        return template