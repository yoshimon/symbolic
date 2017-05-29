"""Contains classes to resolve dependencies in symbolic."""

# Built-in
from enum import Enum
from itertools import chain
from collections import deque, defaultdict
import functools

# Library
import networkx as nx

# Project
from symbolic.exceptions import *
from symbolic.language import *
from symbolic.objects import *
from symbolic.parsers import *
from symbolic.preprocessors import ExternalPreprocessor

class ScopeState(Enum):
    """
    Enumeration of all ExpressionAtom kinds.

    Attributes:
        Default (int): The default scope state.
        Scope (int): The current scope is a loop.
    """

    Default = 0
    Loop = 2

class Dependency:
    """
    A dependency within a project.

    Attributes:
        libName (str): The library that created the dependency.
        locatable (objects.Locatable): The object behind the dependency.
        location (objects.Location): The location of the object in the library.
        references ([objects.Reference]): The references that are seen by this dependency.
        isPrivate (bool): True, if the object is private. Otherwise False.
        isDeprecated (bool): True, if the object is deprecated. Otherwise False.
    """

    def __init__(self, locatable):
        """
        Initialize the object.

        Args:
            locatable (objects.Locatable): The object behind the dependency.
        """
        self.libName = locatable.anchor.libName
        self.locatable = locatable

        # Cache some members from the locatable.
        self.location = locatable.location()
        self.baseLocation = self.location.base()
        self.references = locatable.references
        
        # Two system annotations are valid for all dependencies: private, deprecate
        self.isPrivate = Annotation.has('private', locatable.annotations) if locatable is isinstance(locatable, Named) else False
        self.isDeprecated = Annotation.has('deprecated', locatable.annotations) if locatable is isinstance(locatable, Named) else False

    def __eq__(self, other):
        """
        Return whether two dependencies are identical.
        
        Args:
            other (objects.Dependency): The dependency to compare with.
        Returns:
            bool: True, if both dependencies point to the same library. Otherwise False.
        """
        return self.libName == other.libName and self.baseLocation == other.baseLocation and self.references == other.references

    def __hash__(self):
        """
        Return a hash value for this object.

        Returns:
            int: The hash value.
        """
        h0 = ("{0} {1} {2}".format(str(self.libName), str(self.baseLocation), str(self.references))).__hash__()
        return h0

class LocationConflict:
    """
    A conflict between two locations.
    
    Attributes:
        firstDependency (dag.Dependency): The first dependency.
        secondDependency (dag.Dependency): The second dependency.
    """

    def __init__(self, firstDependency, secondDependency):
        """
        Initialize the object.
        
        Args:
            firstDependency (dag.Dependency): The first dependency.
            secondDependency (dag.Dependency): The second dependency.
        """
        self.firstDependency = firstDependency
        self.secondDependency = secondDependency

class ResolvedDependencyLocation:
    """
    A collection of dependencies that have been resolved to a location.

    Attributes:
        dependencies ([dag.Dependency]): The dependencies at this location.
        subLocations ({str, dag.ResolvedDependencyLocation}): The sub-locations.    
    """

    def __init__(self, dependencies=None, subLocations=None):
        """
        Initialize the object.

        Args:
            dependencies ([dag.Dependency]): The dependencies at this location.
            subLocations ({str, dag.ResolvedDependencyLocation}): The sub-locations.
        """
        self.dependencies = dependencies if dependencies else []
        self.subLocations = subLocations if subLocations else {}

class NavigationQuery:
    """
    Represents a navigation query inside a project dependency collection.

    Attributes:
        _uid (str): The UID of the query.
    """

    def __init__(self, references, parent, location):
        """
        Initialize the object.

        Args:
            references (list): The references list.
            parent (objects.Locatable): The parent locatable object.
            location (objects.Location): The location to find.
        """
        refStr = "[{0}]".format(",".join(str(ref) for ref in references))
        parentStr = str(parent)
        locationStr = str(location)
        self._uid = "{0} {1} {2}".format(refStr, parentStr, locationStr)

    def __str__(self):
        """
        Return a string representation of the object.

        Returns:
            str: The string representation.
        """
        return self._uid

    def __eq__(self, other):
        """
        Return whether two queries are identical.
        
        Args:
            other (objects.NavigationQuery): The query to compare with.
        Returns:
            bool: True, if both queries point to the same library. Otherwise False.
        """
        return self._uid == other._uid

    def __hash__(self):
        """
        Return a hash value for this object.

        Returns:
            int: The hash value.
        """
        return self._uid.__hash__()

class NavigationResult:
    """
    A result of a navigation operation.

    Attributes:
        libName (str): The library name.
        resolvedDependencyLocation ([dag.ResolvedDependencyLocation]): The resolved dependency location.
        dependency (dag.Dependency): The matched dependency.
    """

    def __init__(self, libName, resolvedDependencyLocation, dependency):
        """
        Initialize the object.

        Args:
            libName (str): The library name.
            resolvedDependencyLocation ([dag.ResolvedDependencyLocation]): The resolved dependency location.
            dependency (dag.Dependency): The matched dependency.
        """
        self.libName = libName
        self.resolvedDependencyLocation = resolvedDependencyLocation
        self.dependency = dependency

    def __eq__(self, other):
        """
        Return whether two navigation results point to the same location.
        
        Args:
            other (dag.NavigationResult): The navigation result to compare with.
        Returns:
            bool: True, if both results point to the same location. Otherwise False.
        """
        return self.libName == other.libName and id(self.resolvedDependencyLocation) == id(other.resolvedDependencyLocation)

class AstNavigationResult:
    """
    A navigation result used during the AST validation phase.

    This is a convenience class to extract from a navigation result with an expected dependency match count of exactly one.
    """

    def __init__(self, navResult):
        """
        Initialize the object.

        Args:
            navResult (dag.NavigationResult): The navigation result to wrap.
        """
        self.dependency = navResult.dependency
        self.explicitLocation = Location([RelativeLocation(LocationKind.Reference, navResult.libName)] + self.dependency.location.path)

    def as_array(self, dims):
        """
        Return the navigation result as an array type.

        This function returns a navigation result with the same dependency, but the explicit location
        will have the array dimensions set.

        Returns:
            dag.AstNavigationResult: The same result as an array.
        """
        resolvedDependencyLocation = ResolvedDependencyLocation([self.dependency])
        navResult = NavigationResult(self.explicitLocation[0].name, resolvedDependencyLocation, self.dependency)
        arrayNR = AstNavigationResult(navResult)
        
        # We need to modify the last relative location so copy the path.
        copiedPath = Location([RelativeLocation(p.kind, p.name, templateParameters=p.templateParameters, parameters=p.parameters, dims=p.dims) for p in self.dependency.location.path])
        arrayNR.explicitLocation = copiedPath
        arrayNR.explicitLocation[-1].dims = dims
        
        return arrayNR

    def as_base(self):
        """
        Return the navigation result as a base type without array dimensions.

        This function returns a navigation result with the same dependency, but the explicit location
        will have the array dimensions unset.

        Returns:
            dag.AstNavigationResult: The same result as a base type.
        """
        return self.as_array([])

    def __eq__(self, other):
        """
        Return whether two navigation results point to the same location.
        
        Args:
            other (dag.NavigationResult): The navigation result to compare with.
        Returns:
            bool: True, if both results point to the same location. Otherwise False.
        """
        return self.explicitLocation == other.explicitLocation

class ProjectDependencyCollection:
    """
    A colllection of dependencies within a project.

    Attributes:
        unresolvedDependencies ({dag.Dependency}): A set of unresolved dependencies.
        libraries (dict): The libraries lookup table.
        resolvedObjects (defaultdict): Maps each library to a list of resolved objects
        links (dict): Maps unresolved dependencies to their resolved counterparts
        locationConflicts ([objects.LocationConflict]): A list of location conflicts.
    """

    def __init__(self, userTypeLocationStrings):
        """Initialize the object."""
        self.unresolvedDependencies = list()
        self.libraries = {} # The libraries lookup table
        self.resolvedObjects = defaultdict(ResolvedDependencyLocation) # Maps each library to a set of resolved objects
        self.links = {} # Maps unresolved dependencies to their resolved counterparts.
        self.locationConflicts = [] # Locations that point to the same endpoint (conflicts).
        self.libName = None # The name of the library that is being monitored.
        self.templateLinks = {} # Maps locations to template instantiation results.
        self.functions = [] # An internal cache of function objects inside the current library.
        self._libLocationNavResults = {} # Maps navigation queries to their resolved navigation result.

        # Cache the user-specified locations for numeric types.
        lexer = SymbolicLexer(libName=None, fileName=None)

        # Maps native types to user-specified typename locations
        nativeTypenames = ["int", "float", "bool", "void"]
        validUserTypeLocationStrings = { k: str(v) for k, v in userTypeLocationStrings.items() if k in nativeTypenames }
        self.nativeTypenameLocations = { k: None for k in nativeTypenames }

        for nativeTypename, userLocationString in validUserTypeLocationStrings.items():
            lexer.fileName = "$SystemTypes/{0}".format(nativeTypename)

            # Convert the user location string to a token stream.
            userTypeLocationTokens = lexer.tokenize(userLocationString)

            # Setup a parser to analyze that token stream.
            parser = UnitParser(lexer.libName, lexer.fileName, userTypeLocationTokens)

            # This will only >setup< the root namespace, i.e. an anonymous namespace
            # containing the references within the translation unit.
            # It will not parse any objects within the root namespace.
            parser.parse_root_references_only()

            # The function above parses references but we do not want / allow that.
            # Make sure we didn't actually parse any references.
            if parser.references:
                raise InvalidTypenameError(parser.namespaceStack[-1].anchor)

            # Now try to parse the typename.
            typename = Typename.parse(parser)

            # Make sure nothing follows after the typename.
            # Again, we only want the typename, nothing else.
            if not parser.is_eof():
                raise ExpectedEOFError(typename.anchor)

            # Extract the location from the typename.
            typenameLoc = typename.location()
            if typenameLoc.dims():
                raise NativeTypenameArrayError(str(typenameLoc))

            self.nativeTypenameLocations[nativeTypename] = typenameLoc

        # Initialize function table that maps ExpressionAtomKinds to AST verification functions.
        self._astVerifiers = {
            ExpressionAtomKind.Var: self._verify_ast_var,
            ExpressionAtomKind.Number: self._verify_ast_number,
            ExpressionAtomKind.FunctionBegin: self._verify_ast_function,
            ExpressionAtomKind.ArrayBegin: self._verify_ast_array,
            ExpressionAtomKind.TemplateBegin: self._verify_ast_template,
            ExpressionAtomKind.UnaryOp: self._verify_ast_unary_op,
            ExpressionAtomKind.BinaryOp: self._verify_ast_binary_op
        }

    def _ast_try_navigate_dependency(self, locatable, *, libName=None):
        """
        Try to navigate to a locatable object.

        Args:
            locatable (objects.Locatable): The locatable object to search for.
            libName (str): The library to navigate in.
        Returns:
            dag.AstNavigationResult or None: The navigation result or None.
        """
        dependency = Dependency(locatable)
        navResult = self.try_navigate_dependency(dependency)
        if navResult is None:
            return None

        astNavResult = AstNavigationResult(navResult)

        # Navigate through aliases down to base.
        if astNavResult.explicitLocation.path[-1].kind == LocationKind.Type:
            navResult = self.navigate_alias_base(navResult)
            astNavResult = AstNavigationResult(navResult)
        
        return astNavResult

    def _ast_navigate_dependency(self, locatable, *, libName=None):
        """
        Navigate to a locatable object.

        Args:
            locatable (objects.Locatable): The locatable object to search for.
            libName (str): The library to navigate in.
        Returns:
            dag.AstNavigationResult: The navigation result.
        """
        navResult = self._ast_try_navigate_dependency(locatable, libName=libName)
        if navResult is None:
            raise DependencyNotFoundError(locatable.anchor, locatable.location())
        
        return navResult

    def _try_verify_ast_member(self, atom, lhs):
        """
        Verify a struct member in an AST.

        Args:
            atom (objects.ExpressionAtom): The root atom.
            lhs (dag.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
        Returns:
            dag.AstNavigationResult or None: The location of the resulting type of this AST.
        """
        if lhs is None:
            return None

        locatable = lhs.dependency.locatable
        if isinstance(locatable, Template):
            self.try_navigate(atom.token.anchor, locatable.references, locatable.parent, None)
            # TODO: Instantiate the template.

            locatable = locatable.obj

        memberName = str(atom.token)
        memberTypename = locatable.member_typename(atom.token.anchor, memberName)
        structLib = str(lhs.explicitLocation[0])
        memberNR = self._ast_try_navigate_dependency(memberTypename, libName=structLib)
        return memberNR

    def _verify_ast_var(self, container, atom, children, localVars, newLocalVars, isOptional, lhs):
        """
        Verify an AST with a variable atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (dag.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
        Returns:
            dag.AstNavigationResult: The location of the resulting type of this AST.
        """
        # Try to lookup a membver.
        memberNR = self._try_verify_ast_member(atom, lhs)
        if memberNR:
            return memberNR

        # Handle struct members and extensions.
        if lhs is not None:
            # Try extensions.
            pass

        # Handle boolean values: true, false.
        if atom.token in ["true", "false"]:
            boolTypename = self.native_bool_typename(container.references)
            navResult = self._ast_navigate_dependency(boolTypename)
            return navResult

        # Lookup the resolved variable type.
        varNR = localVars.get(str(atom.token), None)
        if varNR is not None:
            return varNR

        if not isOptional:
            raise VariableNotFoundError(atom.token)

        return None

    def _try_verify_ast_typename(self, references, token, children, lhs):
        """
        Verify an AST with a typename.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (dag.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
        Returns:
            dag.AstNavigationResult: The location of the resulting type of this AST.
        """
        rhsLocation = Location([RelativeLocation(LocationKind.Type, str(token))])
        rhsTypename = Typename.from_location(references, rhsLocation)
        if lhs is not None:
            fullTypename = rhsTypename
        else:
            fullTypename = rhsTypename
        typeNR = self._ast_try_navigate_dependency(fullTypename)
        return typeNR

    def _verify_ast_number(self, container, atom, children, localVars, newLocalVars, isOptional, lhs):
        """
        Verify an AST with a number atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (dag.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
        Returns:
            dag.AstNavigationResult: The location of the resulting type of this AST.
        """
        # Navigate to the numeric type (None stands for INFINITY).
        if atom.token is None or atom.token.isInteger:
            # Int
            typename = self.native_int_typename(container.references)
        elif atom.token.isFloat:
            # Float
            typename = self.native_float_typename(container.references)
        else:
            assert(False)

        result = self._ast_navigate_dependency(typename)
        return result

    def _verify_ast_function(self, container, atom, children, localVars, newLocalVars, isOptional, lhs):
        """
        Verify an AST with a function atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (dag.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
        Returns:
            dag.AstNavigationResult: The location of the resulting type of this AST.
        """
        # Extract child information.
        childNRs = [self._verify_expression_ast_recursive(container, localVars, child, newLocalVars) for child in children]
        childTypenames = [Typename.from_location(container.references, childNR.explicitLocation) for childNR in childNRs]
        childParameters = [Parameter(container, child.atom.token, [], None, childTypenames[i], child.isRef) for i, child in enumerate(children)]

        possibleMatchNR = self._try_find_function(container, atom.token, FunctionKind.Regular, childParameters)

        if possibleMatchNR is not None:
            # Lookup the return type.
            funcRetTypenameNR = self._ast_navigate_dependency(possibleMatchNR.dependency.locatable.returnTypename)
            return funcRetTypenameNR

        raise FunctionOverloadNotFoundError(atom.token, childParameters)

    def _verify_ast_array(self, container, atom, children, localVars, newLocalVars, isOptional, lhs):
        """
        Verify an AST with an array atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (dag.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
        Returns:
            dag.AstNavigationResult: The location of the resulting type of this AST.
        """
        # Extract child information.
        isScalar = len(children) > 0
        for child in children:
            childNR = self._verify_expression_ast_recursive(container, localVars, child, newLocalVars)
            childTypename = Typename.from_location(container.references, childNR.explicitLocation)
        
            # Make sure the child (here: array index) resolves to an integer.
            intTypename = self.native_int_typename(container.references)
            intNR = self._ast_navigate_dependency(intTypename)

            if childNR != intNR:
                raise InvalidArrayIndexTypeError(atom.token.anchor)

            if str(child.atom.token) != "1":
                isScalar = False

        # Return the type of the underlying object.

        # Try it as a variable first.
        varNR = self._verify_ast_var(container, atom, children, localVars, newLocalVars, True, lhs)
        if varNR:
            # Make sure the indexing matches the type dimensions.
            varTypeDims = varNR.explicitLocation[-1].dims
            expectedDims = max(1, len(varTypeDims))
            if len(children) != expectedDims:
                raise InvalidArrayIndexDimensionsError(atom.token.anchor, expectedDims)

            # Remove array bounds.
            baseTypeNR = varNR.as_base()
            return baseTypeNR

        # Try it as a member first.
        memberNR = self._try_verify_ast_member(atom, lhs)
        if memberNR:
            return memberNR

        # TODO: Now try it as an extension.

        # Try it as a typename.
        baseTypeNR = self._try_verify_ast_typename(container.references, atom.token, children, lhs)
        if baseTypeNR is not None:
            # int[1,1,1...] == int
            if isScalar:
                return baseTypeNR

            # Deduce array dimensions and append to typename (array declaration).
            dims = []
            for child in children:
                cat = child.atom.token
                if cat is not None and cat.kind != Token.Number.Integer:
                    raise InvalidArrayIndexTypeError(cat.anchor)

                if cat is not None:
                    dim = int(str(cat))
                    if dim < Language.minArrayDim:
                        raise InvalidArrayDimensionsError(cat.anchor)
                else:
                    dim = None

                dims.append(dim)

            # Create a new typename.
            arrayTypeNR = baseTypeNR.as_array(dims)
            return arrayTypeNR

        if lhs is None:
            raise MissingArrayTypeError(atom.token.anchor)

        assert(False)

    def native_typename(self, references, name):
        """
        Return the native int typename.

        Args:
            references (list): The references to associate with the typename.
            name (str): The native type name.
        Returns:
            Typename: The typename.
        """
        return Typename.from_location(references, self.nativeTypenameLocations[name])

    def native_int_typename(self, references):
        """
        Return the native int typename.

        Args:
            references (list): The references to associate with the typename.
        Returns:
            Typename: The typename.
        """
        return self.native_typename(references, "int")

    def native_float_typename(self, references):
        """
        Return the native float typename.

        Args:
            references (list): The references to associate with the typename.
        Returns:
            Typename: The typename.
        """
        return self.native_typename(references, "float")

    def native_bool_typename(self, references):
        """
        Return the native bool typename.

        Args:
            references (list): The references to associate with the typename.
        Returns:
            Typename: The typename.
        """
        return self.native_typename(references, "bool")

    def _verify_ast_template(self, container, atom, children, localVars, newLocalVars, isOptional, lhs):
        """
        Verify an AST with a template atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (dag.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
        Returns:
            dag.AstNavigationResult: The location of the resulting type of this AST.
        """
        pass

    def _verify_ast_unary_op(self, container, atom, children, localVars, newLocalVars, isOptional, lhs):
        """
        Verifies an AST with a unary operator atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (dag.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
        Returns:
            dag.AstNavigationResult: The location of the resulting type of this AST.
        """
        assert(len(children) == 1)

        # Extract child information.
        child = children[0]
        childNR = self._verify_expression_ast_recursive(container, localVars, child, newLocalVars)
        childTypename = Typename.from_location(container.references, childNR.explicitLocation)

        # Lookup the operator.
        childParameter = Parameter(container, child.atom.token, [], None, childTypename, child.is_lvalue())
        possibleMatchNR = self._try_find_function(container, atom.token, FunctionKind.Operator, [childParameter])

        if possibleMatchNR is None:
            # Try it again with non-ref versions.
            childParameter.isRef = not childParameter.isRef
            possibleMatchNR = self._try_find_function(container, atom.token, FunctionKind.Operator, [childParameter])

        if possibleMatchNR is not None:
            # Lookup the return type.
            funcRetTypenameNR = self._ast_navigate_dependency(possibleMatchNR.dependency.locatable.returnTypename)
            return funcRetTypenameNR

        raise UnaryOperatorOverloadNotFoundError(atom.token, childTypename)

    def _try_find_function(self, container, nameToken, kind, parameters):
        """
        Try to find a function matching a given signature.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            nameToken (lexer.Symto): A token with the name of the function to look for.
            kind (objects.FunctionKind): The function kind to look for.
            parameters (list of objects.Parameter): The parameter signature.
        Returns:
            dag.AstNavigationResult: The location of the resulting type of this AST.
        """
        locatable = Function(container.references, container, nameToken, [], None, None, kind, None, None, parameters, None, None)
        navResult = self._ast_try_navigate_dependency(locatable)
        return navResult

    def _verify_ast_binary_op(self, container, atom, children, localVars, newLocalVars, isOptional, lhs):
        """
        Verify an AST with a binary operator atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (dag.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
        Returns:
            dag.AstNavigationResult: The location of the resulting type of this AST.
        """
        assert(len(children) == 2)

        left, right = children[0], children[1]

        isNewVarOp = atom.token == ":="
        isStructOp = atom.token == "."
            
        leftNR = self._verify_expression_ast_recursive(container, localVars, left, newLocalVars, isOptional=isNewVarOp, lhs=lhs)
        leftResolved = leftNR if isStructOp else None
        rightNR = self._verify_expression_ast_recursive(container, localVars, right, newLocalVars, lhs=leftResolved)

        if isStructOp:
            return rightNR

        # Is the LHS a new variable?
        if isNewVarOp:
            if left.atom.kind != ExpressionAtomKind.Var:
                raise LValueRequiredError(atom.token)

            # It has to be a new local variable.
            varToken = left.atom.token
            varName = str(varToken)
            if varName in newLocalVars:
                raise VariableAlreadyExistsError(varToken)

            if varName in localVars:
                # The types have to match.
                # As agreed upon by the committee (reviewed by NightCreature). 
                existingVarTypeNR = localVars[varName]
                if existingVarTypeNR != rightNR:
                    raise VariableTypeMismatchError(varToken.anchor)

            newLocalVars[varName] = rightNR
            return rightNR
        else:
            # Build typenames for the resolved locations.
            leftTypename = Typename.from_location(container.references, leftNR.explicitLocation)
            rightTypename = Typename.from_location(container.references, rightNR.explicitLocation)

            # Turn them into parameters.
            pLeft = Parameter(container, left.atom.token, [], None, leftTypename, left.is_lvalue())
            pRight = Parameter(container, right.atom.token, [], None, rightTypename, right.is_lvalue())
                    
            # Try to find a match for the signature.
            possibleMatchNR = self._try_find_function(container, atom.token, FunctionKind.Operator, [pLeft, pRight])
            if possibleMatchNR is None:
                # Try it again with non-ref versions.
                pLeft.isRef = not pLeft.isRef
                possibleMatchNR = self._try_find_function(container, atom.token, FunctionKind.Operator, [pLeft, pRight])
                if possibleMatchNR is None:
                    pRight.isRef = not pRight.isRef
                    possibleMatchNR = self._try_find_function(container, atom.token, FunctionKind.Operator, [pLeft, pRight])
                    if possibleMatchNR is None:
                        pLeft.isRef = not pLeft.isRef
                        possibleMatchNR = self._try_find_function(container, atom.token, FunctionKind.Operator, [pLeft, pRight])

            if possibleMatchNR is not None:
                # Lookup the return type.
                funcRetTypenameNR = self._ast_navigate_dependency(possibleMatchNR.dependency.locatable.returnTypename)
                return funcRetTypenameNR

            raise BinaryOperatorOverloadNotFoundError(atom.token, pLeft, pRight)

    def try_navigate(self, errorAnchor, references, parent, location):
        """
        Navigate to a location.

        Args:
            errorAnchor (lexer.Anchor): The anchor to use in case an exception is thrown.
            references ([objects.Reference]): The references to use for the navigation.
            parent (objects.Locatable or None): The parent object to start the search at.
                The navigation will start the the parent and, on failure, move up the
                hierarchy until a match is found.
            location (objects.Location): The location.
        Returns:
            dag.NavigationResult or None: The resolved location or None, if no match was found.
        """
        # Lookup from ID cache.
        navQuery = NavigationQuery(references, parent, location)
        result = self._libLocationNavResults.get(str(navQuery), None)
        if result is not None:
            return result

        # Navigate the library tree first (explicit library name).
        # Fall back to reference order in unit, if no match possible.
        offset = 0 # The offset where the object name begins
        lookup = self.libraries
        for i, rl in enumerate(location):
            # The relative location has to exist
            if rl.name not in lookup:
                break

            # The relative location has to be plain
            if not rl.is_plain():
                raise InvalidReferenceUsageError(errorAnchor)

            lookup = lookup[rl.name]
            offset = i + 1
        
        # Implicit or explicit lookup
        isImplicitRef = offset == 0
        libNameGen = chain([self.libName], (str(ref) for ref in references)) if isImplicitRef else ['.'.join(rl.name for rl in location[:offset])]

        # Strip the library name
        locationWithoutLibName = location[offset:]
        relativeLocationPath = [locationWithoutLibName[-1]] if locationWithoutLibName else []

        # Generate a sequence of additional locations to search at.
        # This will search the parent location, and all of its parents first.
        locationsWithoutLibName = deque()
        currentParent = parent
        while (currentParent is not None) and (currentParent.parent is not None):
            # Prepend the location of the parent to the search path.
            parentPath = currentParent.location().path
            otherLocationWithoutLibName = Location(parentPath + relativeLocationPath)
            locationsWithoutLibName.append(otherLocationWithoutLibName)
            currentParent = currentParent.parent

        if location:
            locationsWithoutLibName.append(Location([location[-1]]))

        # Find the library and the object in the library
        lookup = None
        resolvedLibName = None
        locationFound = False
        matchedDependency = None
        for libName in libNameGen:
            # Assume this library contains the dependency
            resolvedLibName = libName
            baseLookup = self.resolvedObjects[libName]
            lookup = baseLookup

            # Loop through all sublocations and verify the assumption above
            allSubLocationsFound = True
            for locationWithoutLibName in locationsWithoutLibName:
                allSubLocationsFound = True
                lookup = baseLookup
                for i, rl in enumerate(locationWithoutLibName):
                    # Assume the assumption above is false.
                    allSubLocationsFound = False

                    if rl.name not in lookup.subLocations:
                        break

                    # The dependencies associated with this name.
                    resolvedDependencyLocation = lookup.subLocations[rl.name]
                    dependencies = resolvedDependencyLocation.dependencies

                    # Perform ADL to filter out potential matches.
                    possibleMatches = filter(lambda dependency: i < len(dependency.baseLocation) and rl.might_be_equal_to(dependency.baseLocation[i]), dependencies)
                    if not possibleMatches:
                        continue

                    # Find the best match out of the possible matches by counting the number of
                    # partial matches. The highest match count wins.
                    countPartialMatches = lambda templateParameters: functools.reduce(lambda numMatches, p: numMatches + 1 if p.partialMatch is not None else 0, templateParameters, 0) 
                    dependencyCmp = lambda a, b: countPartialMatches(b.baseLocation[i].templateParameters) - countPartialMatches(a.baseLocation[i].templateParameters)
                    bestMatches = sorted(possibleMatches, key=functools.cmp_to_key(dependencyCmp))

                    for dependency in bestMatches:
                        dependencyRL = dependency.baseLocation[i]
                        dependencyLocatable = dependency.locatable

                        if isinstance(dependencyLocatable, Function):
                            # Validate requested and found signature.
                            requestedParams = rl.parameters
                            foundParams = dependencyRL.parameters

                            # Both lists are guaranteed to be the same length here.
                            # We just need to check the target locations.
                            assert(len(requestedParams) == len(foundParams))
                            parameterMismatch = False
                            for paramIndex, paramA in enumerate(requestedParams):
                                paramB = foundParams[paramIndex]

                                # Reference qualifiers have to match.
                                if paramA.isRef != paramB.isRef:
                                    parameterMismatch = True
                                    break

                                paramTypenameA = paramA.typename
                                paramTypenameB = paramB.typename

                                # Array dimensions have to match.
                                if paramTypenameA.dims != paramTypenameB.dims:
                                    parameterMismatch = True
                                    break

                                paramDepA = Dependency(paramTypenameA)
                                paramDepB = Dependency(paramTypenameB)

                                # Both typenames must resolve to the same location.
                                paramANR = self.navigate_dependency(paramDepA)
                                paramBNR = self.navigate_dependency(paramDepB)
                                
                                paramANR = self.navigate_alias_base(paramANR)
                                paramBNR = self.navigate_alias_base(paramBNR)

                                if paramANR != paramBNR:
                                    parameterMismatch = True
                                    break

                            if parameterMismatch:
                                continue
                        elif isinstance(dependencyLocatable, Alias):
                            # If it is an alias then we have to lookup the aliased type instead
                            # since aliases don't have any sublocations.
                            # But only do that if this is not the last relative location
                            # since we are actually looking for the Alias in that case.
                            if i != len(location.path) - 1:
                                aliasNavResult = NavigationResult(resolvedLibName, resolvedDependencyLocation)
                                aliasNavResult = self.navigate_alias_base(aliasNavResult)
                                libName = aliasNavResult.libName
                                resolvedDependencyLocation = aliasNavResult.resolvedDependencyLocation
                        elif isinstance(dependencyLocatable, Template):
                            # Lazily instantiate templates, if we encounter one.
                            # Make sure we did not instantiate the template already.
                            templateInstanceArgs =  ", ".join(str(p) for p in rl.templateParameters)
                            importLibs = ", ".join(str(ref) for ref in references)
                            importLibs = " using {0}".format(importLibs) if importLibs else importLibs
                            dependencyLocationStr = "{0} with <{1}>{2}".format(str(dependency.baseLocation), templateInstanceArgs, importLibs)

                            if dependencyLocationStr not in self.templateLinks:
                                # Generate the translation unit for the template.
                                templateSrc = dependencyLocatable.generate_translation_unit()

                                # Run the pre-processor on the source.
                                # TODO: run pre-processor based on the location
                                ppTemplateSrc = templateSrc # self.preprocessor.run(templateSrc, LIBNAME, PPT)

                                # Lex the unit so we can parse it.
                                lexer = SymbolicLexer(libName=self.libName, fileName="$Templates/{0}".format(str(dependencyLocatable.token)))

                                # Plugin the template substitutions for the lexer.
                                templateSubs = { str(dependencyRL.templateParameters[i].token): str(parameter.token)[1:-1] for i, parameter in enumerate(rl.templateParameters) }

                                # Generate a parsable token stream now.
                                srcFileTokens = lexer.tokenize(ppTemplateSrc, subs=templateSubs)
                                srcFileTokens = lexer.concatenate_tokens(srcFileTokens)

                                # Analyze the token stream.
                                parser = UnitParser(lexer.libName, lexer.fileName, srcFileTokens)
                                rootNamespace = parser.parse()

                                # Lookup the template object
                                templateObj = rootNamespace.locatables[0]
                                templateObj.parent = dependencyLocatable.parent
                                templateObj.grandParent = dependencyLocatable.grandParent
                                templateObj.grandParentWithoutRoot = dependencyLocatable.grandParentWithoutRoot

                                # Bind the location to a template.
                                self.templateLinks[dependencyLocationStr] = templateObj

                                # Insert it into the collection so we can look it up.
                                # Update the references to match the call site.
                                rootNamespace.references = references
                                self.insert_unit(rootNamespace)
                        
                            # Use template links to jump to the right location, which is anonymous.
                            templateDep = Dependency(self.templateLinks[dependencyLocationStr])
                            templateNavResult = self.navigate_dependency(templateDep)
                            templateAliasNavResult = self.navigate_alias_base(templateNavResult)
                            resolvedDependencyLocation = templateAliasNavResult.resolvedDependencyLocation

                        # Step down this namespace
                        allSubLocationsFound = True
                        lookup = resolvedDependencyLocation
                        matchedDependency = dependency
                        break

                if allSubLocationsFound:
                    break

            # If we verified the location we can stop the search.
            locationFound = allSubLocationsFound
            if locationFound:
                break

        if not locationFound:
            return None

        result = NavigationResult(resolvedLibName, lookup, matchedDependency)

        # Cache the result.
        # This is used to lookup function parameter typename locations later.
        self._libLocationNavResults[str(navQuery)] = result

        return result

    def navigate(self, errorAnchor, references, parent, location):
        """
        Navigate to a location.

        Args:
            errorAnchor (lexer.Anchor): The anchor to use in case an exception is thrown.
            references ([objects.Reference]): The references to use for the navigation.
            parent (objects.Locatable or None): The parent object to start the search at.
                The navigation will start the the parent and, on failure, move up the
                hierarchy until a match is found.
            location (objects.Location): The location.
        Returns:
            dag.NavigationResult: The resolved location.
        """
        navResult = self.try_navigate(errorAnchor, references, parent, location)
        if navResult is None:
            raise DependencyNotFoundError(errorAnchor, location)
        return navResult

    def navigate_parent(self, dependency):
        """
        Navigate to the parent of a dependency.

        Args:
            dependency (dag.Dependency): The dependency to find the parent of.
        Returns:
            dag.NavigationResult: The navigation result.
        """
        locatable = dependency.locatable
        location = dependency.baseLocation
        return self.navigate(locatable.anchor, locatable.references, locatable.grandParentWithoutRoot, location[:-1])

    def insert(self, locatable):
        """
        Insert an object into the dependency collection.

        Args:
            locatable (objects.Locatable): The object to insert.
        """
        # Create and cache the dependency
        dependency = Dependency(locatable)
        rl = dependency.baseLocation[-1]

        if rl.kind == LocationKind.Unresolved:
            self.unresolvedDependencies.append(dependency)
        else:
            # Navigate to parent
            navResult = self.navigate_parent(dependency)
            lookup = navResult.resolvedDependencyLocation.subLocations

            # Make sure that no duplicate object in this namespace exists
            if rl.name in lookup:
                existingDependencies = lookup[rl.name].dependencies

                # See if parameter signature and template parameter count matches
                for otherDependency in existingDependencies:
                    otherDependencyRL = otherDependency.baseLocation[-1]

                    # If the location kinds are different then it is ambigous
                    #if rl.kind != otherDependencyRL.kind:
                    #    raise DuplicateNameError(locatable.anchor, otherDependency.locatable.anchor)

                    # If the template and signature matches then it might be a conflict
                    if rl.might_be_equal_to(otherDependencyRL):
                        # Partial matches must match exactly.
                        isExactPartialMatch = Algorithm.zip_all(rl.templateParameters, otherDependencyRL.templateParameters,
                                                                lambda p0, p1: p0.partialMatch == p1.partialMatch)
                        if isExactPartialMatch:
                            conflict = LocationConflict(dependency, otherDependency)
                            self.locationConflicts.append(conflict)

                # Register dependency at this location
                existingDependencies.append(dependency)
            else:
                lookup[rl.name] = ResolvedDependencyLocation([dependency], {})

    def insert_unit(self, rootNamespace):
        """
        Insert an unresolved translation unit into the collection.

        Args:
            rootNamespace (objects.Namespace): The global namespace.
        """
        # Create a dependency for every object
        references = rootNamespace.references
        locatables = deque(rootNamespace.locatables)
        while locatables:
            locatable = locatables.popleft()

            # Register it
            self.insert(locatable)

            # Recursive objects (Namespaces, Functions)
            children = getattr(locatable, "locatables", None)
            locatables += children if children is not None else []

            # Connect immediate unresolved dependencies
            if isinstance(locatable, Function):
                # The unknown return type
                self.insert(locatable.returnTypename)

                # The unknown parameter types
                for parameter in locatable.parameters:
                    self.insert(parameter.typename)

                # Remember this function for instruction verification later.
                self.functions.append(locatable)
            elif isinstance(locatable, Alias):
                # The unknown typename
                self.insert(locatable.targetTypename)
            elif isinstance(locatable, MemberList):
                # Unknown member types.
                self.insert(locatable.typename)

    def begin_library(self, libName):
        """
        Begin collecting dependencies for a new library.
        
        Args:
            libName (str): The name of the library.
        """
        print()
        print("-" * 80)
        print("Building {0}".format(libName))
        print("-" * 80)

        # Create an empty entry in the global dict
        # Breakup the library name and insert it
        # Each part of the library name is its own subspace
        # e.g. std.types and std.math would share the std subspace
        strings = libName.split('.')
        lookup = self.libraries
        for s in strings:
            # Create new subspace
            if s not in lookup:
                lookup[s] = {}

            # Step down
            lookup = lookup[s]

        self.libName = libName
        self.functions = []
        self._libLocationNavResults = {}

    def end_library(self):
        """End collecting dependencies for the current library."""
        self._resolve()

        # Close this library
        self.libName = None
        self.functions = []

    def _resolve(self):
        """Resolve all dependencies."""
        if self.unresolvedDependencies:
            # Traverse from top to bottom
            unresolvedDependencies = deque(self.unresolvedDependencies)
            
            # Clear the unresolved dependencies, they are in the queue
            self.unresolvedDependencies.clear()
            
            while unresolvedDependencies:
                dependency = unresolvedDependencies.popleft()

                # The name has to be resolvable by now
                # Catching unresolved objects will spawn templates, if encountered
                self.links[dependency] = self.navigate_dependency(dependency)

                # Add the unresolved dependencies to the queue
                unresolvedDependencies += self.unresolvedDependencies

                # And clear the cache again
                self.unresolvedDependencies.clear()

            # Now that each dependency has been resolved
            # we solve location conflicts due to clashing parameter
            # signatures by comparing the types
            self._solve_location_conflicts()

        # Now that all members and function parameters are resolved we can look at the instructions within
        # the functions.
        self._verify_functions()

    def _verify_functions(self):
        """Verify all instructions within all instantiated functions of the current library."""
        for func in self.functions:
            self._verify_function(func)

    def _verify_function(self, func):
        """
        Verify the contents of a given function body.
        
        Args:
            func (objects.Function): The function object.
        """
        # The local variable cache.
        # Maps each variable name to a resolved type location.
        localVars = {}

        # Push the function parameters as local variables.
        for p in func.parameters:
            pNR = self._ast_navigate_dependency(p.typename)
            localVars[str(p.token)] = pNR

        scopeState = ScopeState.Default
        prevInstruction = None

        for locatable in func.locatables:
            if isinstance(locatable, Instruction):
                self._verify_instruction(func, localVars, locatable, scopeState, prevInstruction)
                prevInstruction = locatable

    def _verify_instruction(self, container, localVars, instruction, scopeState, prevInstruction):
        """
        Verify an instruction.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            localVars (dict): The local variables.
            instruction (objects.Instruction): The instruction to verify.
            scopeState (dag.ScopeState): The current scope state.
            prevInstruction (objects.Instruction): The previous instruction in the same scope.
        """
        # Verify the AST.
        if instruction.kind == InstructionKind.Expression:
            # We don't care about the expression type.
            self._verify_expression_ast(container, localVars, instruction.expression.ast)
        elif instruction.kind in [InstructionKind.Break, InstructionKind.Continue]:
            # Make sure we are inside a loop.
            if scopeState != ScopeState.Loop:
                raise NotInsideLoopError(instruction.anchor)
        elif instruction.kind == InstructionKind.Return:
            exprTypeNR = self._verify_expression_ast(container, localVars, instruction.expression.ast)
            
            # Make sure the expression type matches the function return type.
            func = instruction.parent
            funcRetNR = self._ast_navigate_dependency(func.returnTypename)
            if funcRetNR != exprTypeNR:
                raise ReturnTypeMismatchError(instruction.token.anchor)
        elif instruction.kind in [InstructionKind.If, InstructionKind.While, InstructionKind.Elif]:
            # Verify instruction.
            if instruction.kind == InstructionKind.Elif:
                if prevInstruction is None or prevInstruction.kind not in [InstructionKind.If, InstructionKind.Elif]:
                    raise InvalidElifError(instruction.token.anchor)

            exprType = self._verify_expression_ast(container, localVars, instruction.expression.ast)

            # Make sure the return type evaluates to bool.
            nativeBoolType = self.native_bool_typename(container.references)
            nativeBoolNR = self._ast_navigate_dependency(nativeBoolType)
            if exprType != nativeBoolNR:
                raise PredicateExpectedError(instruction.token.anchor)

            childScopeState = ScopeState.Loop if instruction.kind == InstructionKind.While else ScopeState.Default
            self._verify_child_instructions(container, localVars, instruction, childScopeState)
        elif instruction.kind == InstructionKind.Else:
            # We need an if or elif above.
            if prevInstruction is None or prevInstruction.kind not in [InstructionKind.If, InstructionKind.Elif]:
                raise InvalidElseError(instruction.token.anchor)

            self._verify_child_instructions(container, localVars, instruction, scopeState)
        elif instruction.kind == InstructionKind.For:
            for forInit in instruction.forInits:
                self._verify_expression_ast(container, localVars, forInit.ast)

            for forPred in instruction.forPredicates:
                self._verify_expression_ast(container, localVars, forPred.ast)

            for forStep in instruction.forSteps:
                self._verify_expression_ast(container, localVars, forStep.ast)

            self._verify_child_instructions(container, localVars, instruction, ScopeState.Loop)

    def _verify_child_instructions(self, container, localVars, instruction, scopeState):
        """
        Verify all child instructions of an instruction.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            localVars (dict): The local variables.
            instruction (objects.Instruction): The instruction to verify.
            scopeState (dag.ScopeState): The current scope state.
            prevInstruction (objects.Instruction): The previous instruction in the same scope.
        """
        prevInstruction = None
        for locatable in instruction.instructions:
            self._verify_instruction(container, localVars, locatable, scopeState, prevInstruction)
            prevInstruction = locatable

    def _verify_expression_ast(self, container, localVars, ast):
        """
        Verify an expression.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            localVars (dict): Visible variable declarations.
            ast (objects.ExpressionAST): The expression AST to verify.
        Returns:
            dag.NavigationResult or None: The navigation result after searching for the type of the expression.
        """
        newLocalVars = {}

        result = self._verify_expression_ast_recursive(container, localVars, ast, newLocalVars)
        localVars.update(newLocalVars)

        return result

    def _verify_expression_ast_recursive(self, container, localVars, ast, newLocalVars, *, isOptional=False, lhs=None):
        """
        Verify an expression.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            localVars (dict): Visible variable declarations.
            ast (objects.ExpressionAST): The expression AST to verify.
            newLocalVars (dict): New local variables.
            isOptional (bool): Indicates whether the result of this function can be optionally None.
            lhs (dag.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
        Returns:
            dag.NavigationResult or None: The navigation result after searching for the type of the expression.
        """
        atom = ast.atom
        children = ast.children

        # Validate the AST.
        assert(atom.kind in self._astVerifiers)

        # Invoke the verification handler.
        return self._astVerifiers[atom.kind](container, atom, children, localVars, newLocalVars, isOptional, lhs)

    def navigate_alias_target(self, navResult):
        """
        Navigate to the next target type of an alias.
        
        This can be used after navigating to a location to find the next target type.

        Args:
            navResult (dag.NavigationResult): The previous navigation result to continue the search from.
        Returns:
            dag.NavigationResult or None: The next navigation result or None if there was no resolvable alias.
        """
        if navResult is None:
            return None

        # There should only be one alias dependency at this location.
        dependencies = navResult.resolvedDependencyLocation.dependencies
        dependency = next((dependency for dependency in dependencies if isinstance(dependency.locatable, Alias)), None)
        if dependency is None:
            return None

        # Navigate to the target type.
        targetTypenameDependency = Dependency(dependency.locatable.targetTypename)
        return self.navigate_dependency(targetTypenameDependency)

    def navigate_alias_base(self, navResult):
        """
        Navigate to the base type of an alias.

        This can be used after navigating to a location to find the last target type.

        Args:
            navResult (dag.NavigationResult): The previous navigation result to continue the search from.
        Returns:
            dag.NavigationResult: The next navigation result.
        """
        result = navResult
        while True:
            nextResult = self.navigate_alias_target(result)
            if nextResult is None:
                return result
            
            result = nextResult

    def try_navigate_dependency(self, dependency):
        """
        Navigate using a dependency as the target.

        Args:
            dependency (dag.Dependency): The dependency to lookup.
        Returns:
            dag.NavigationResult: The navigation result.
        """
        return self.links[dependency] if dependency in self.links else self.try_navigate(dependency.locatable.anchor, dependency.locatable.references, dependency.locatable.parent, dependency.baseLocation)

    def navigate_dependency(self, dependency):
        """
        Navigate using a dependency as the target.

        Args:
            dependency (dag.Dependency): The dependency to lookup.
        Returns:
            dag.NavigationResult or None: The navigation result or None, if not match was found.
        """
        navResult = self.try_navigate_dependency(dependency)
        if navResult is None:
            raise DependencyNotFoundError(dependency.locatable.anchor, dependency.location)
        return navResult

    def _solve_location_conflicts(self):
        """Solve all known location conflicts."""
        # Try to resolve location conflicts now.
        for conflict in self.locationConflicts:
            # Look at both conflicts:
            # They have to be parameter conflicts, i.e. f(int) and f(int).
            dep0 = conflict.firstDependency
            dep1 = conflict.secondDependency

            dep0Params = dep0.baseLocation[-1].parameters
            dep1Params = dep1.baseLocation[-1].parameters

            # Assume this is a conflict
            isConflict = True

            # Look at all parameter types in the conflict signatures and
            # lookup the resolved locations for the types.
            for p0, p1 in zip(dep0Params, dep1Params):
                # Create a dependency for the parameters
                p0Dep = Dependency(p0)
                p1Dep = Dependency(p1)

                # Try to find the actual dependency location.
                p0Resolved = self.navigate_dependency(p0Dep)
                p1Resolved = self.navigate_dependency(p1Dep)

                # Navigate aliases down to the base type, if the dependency was an alias.
                # This allows us so to compare the locations for a conflict below.
                p0Resolved = self.navigate_alias_base(p0Resolved)
                p0ResolvedLocation = p0Resolved.resolvedDependencyLocation
                
                p1Resolved = self.navigate_alias_base(p1Resolved)
                p1ResolvedLocation = p1Resolved.resolvedDependencyLocation

                # If the resolved locations differ this is not a conflict.
                if p0ResolvedLocation != p1ResolvedLocation:
                    isConflict = False
                    break

            if isConflict:
                raise DuplicateParameterSignatureError(dep0.locatable.anchor, dep1.locatable.anchor)

        self.locationConflicts = []

class ProjectDependencyGraph:
    pass