"""Contains classes to resolve dependencies in symbolic."""

from enum import Enum
from itertools import chain
from collections import deque, defaultdict
import functools

import networkx as nx

from symbolic.exceptions import *
from symbolic.language import *
from symbolic.objects import *
from symbolic.parsers import *
from symbolic.preprocessors import ExternalPreprocessor

class ScopeState(Enum):
    """
    Enumeration of all scope states inside a function.

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
        baseLocation (objects.Location): The location of the object in the library without array bounds.
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
        self.baseLocationWithoutRef = Location(self.baseLocation.pathWithoutRef)
        self.references = locatable.references

        # Two system annotations are valid for all dependencies: private, deprecate
        self.isPrivate = Annotation.has(Language.private, locatable.annotations) if locatable is isinstance(locatable, Named) else False
        self.isDeprecated = Annotation.has(Language.deprecated, locatable.annotations) if locatable is isinstance(locatable, Named) else False

        # Internal caches.
        self._hash = None
        self._linkableProject = None
        self._resolvedParameterLocations = []

    def __eq__(self, other):
        """
        Return whether two dependencies are identical.

        Args:
            other (objects.Dependency): The dependency to compare with.
        Returns:
            bool: True, if both dependencies point to the same library. Otherwise False.
        """
        return self.libName == other.libName and self.location == other.location and self.references == other.references

    def __hash__(self):
        """
        Return a hash value for this object.

        Returns:
            int: The hash value.
        """
        # Lazily compute and cache hash.
        if self._hash == None:
            self._hash = str(self).__hash__()

        return self._hash

    def __str__(self):
        """
        Return a unique string descriptor for the dependency.

        Returns:
            str: The location string.
        """
        return "{0} using {1}".format(str(self.location), NavigationQuery.references_to_string(self.references))

    def resolve_parameter_locations(self, linkableProject):
        """
        Return a generator sequence for the resolved parameter types.

        Args:
            linkableProject (linker.LinkableProject): The dependency collection to resolve the parameter with.
        Returns:
            list: The resolved parameter types.
        """
        if self._linkableProject != linkableProject:
            self._linkableProject = linkableProject
            self._resolvedParameterLocations = []

            # Rebuild cache.
            if linkableProject is not None:
                for p in self.baseLocation[-1].parameters:
                    pDep = Dependency(p)
                    pResolved = linkableProject.navigate_dependency(pDep)
                    pResolvedBase = linkableProject.navigate_alias_base(pResolved)
                    pResolvedLocation = pResolvedBase.resolvedDependencyLocation
                    self._resolvedParameterLocations.append(pResolvedLocation)

        return self._resolvedParameterLocations

    def as_array(self, dims):
        """
        Return the navigation result as an array type.

        This function returns a navigation result with the same dependency, but the explicit location
        will have the array dimensions set.

        Returns:
            linker.Dependency: The same dependency as an array.
        """
        result = Dependency(self.locatable)
        result.location = Location([RelativeLocation(p.kind, p.name, templateParameters=p.templateParameters, parameters=p.parameters, dims=p.dims) for p in self.location])
        result.location[-1].dims = dims
        return result

class LocationConflict:
    """
    A conflict between two locations.

    Attributes:
        firstDependency (linker.Dependency): The first dependency.
        secondDependency (linker.Dependency): The second dependency.
    """

    def __init__(self, firstDependency, secondDependency):
        """
        Initialize the object.

        Args:
            firstDependency (linker.Dependency): The first dependency.
            secondDependency (linker.Dependency): The second dependency.
        """
        self.firstDependency = firstDependency
        self.secondDependency = secondDependency

class ResolvedDependencyLocation:
    """
    A resolved location for a dependency.

    The location might be shared by other dependencies and it may contain sub-locations.

    Attributes:
        dependencies ([linker.Dependency]): The dependencies at this location.
        subLocations ({str, linker.ResolvedDependencyLocation}): The sub-locations.
    """

    def __init__(self, dependencies=None, subLocations=None):
        """
        Initialize the object.

        Args:
            dependencies ([linker.Dependency]): The dependencies at this location.
            subLocations ({str, linker.ResolvedDependencyLocation}): The sub-locations.
        """
        self.dependencies = dependencies if dependencies is not None else []
        self.subLocations = subLocations if subLocations is not None else {}

class NavigationQuery:
    """
    Represents a navigation query inside a project dependency collection.

    Attributes:
        _uid (str): The UID of the query.
    """

    @staticmethod
    def references_to_string(references):
        """
        Convert a references list to a string representation.

        Returns:
            str: The string representation of the list.
        """
        return "[{0}]".format(",".join(str(ref) for ref in references))

    def __init__(self, references, parent, location):
        """
        Initialize the object.

        Args:
            references (list): The references list.
            parent (objects.Locatable): The parent locatable object.
            location (objects.Location): The location to find.
        """
        refStr = NavigationQuery.references_to_string(references)
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
        resolvedDependencyLocation ([linker.ResolvedDependencyLocation]): The resolved dependency location.
        dependency (linker.Dependency): The matched dependency.
    """

    def __init__(self, resolvedDependencyLocation, dependency):
        """
        Initialize the object.

        Args:
            resolvedDependencyLocation ([linker.ResolvedDependencyLocation]): The resolved dependency location.
            dependency (linker.Dependency): The matched dependency.
        """
        self.resolvedDependencyLocation = resolvedDependencyLocation
        self.dependency = dependency

    def __eq__(self, other):
        """
        Return whether two navigation results point to the same location.

        Args:
            other (linker.NavigationResult): The navigation result to compare with.
        Returns:
            bool: True, if both results point to the same location. Otherwise False.
        """
        return id(self.resolvedDependencyLocation) == id(other.resolvedDependencyLocation) and self.dependency == other.dependency

class AstNavigationResult:
    """
    A navigation result used during the AST validation phase.

    This is a convenience class to extract from a navigation result with an expected dependency match count of exactly one.
    """

    def __init__(self, navResult, isLHSType):
        """
        Initialize the object.

        Args:
            navResult (linker.NavigationResult): The navigation result to wrap.
            isLHSType (bool): True, if this is a LHS type. Otherwise False.
        """
        self.dependency = navResult.dependency
        explicitLoc = self.dependency.location.path
        if explicitLoc[0].kind != LocationKind.Reference:
            explicitLoc = [RelativeLocation(LocationKind.Reference, navResult.dependency.libName)] + explicitLoc

        self.explicitLocation = Location(explicitLoc)
        self.isLHSType = isLHSType

    def as_array(self, dims):
        """
        Return the navigation result as an array type.

        This function returns a navigation result with the same dependency, but the explicit location
        will have the array dimensions set.

        Returns:
            linker.AstNavigationResult: The same result as an array.
        """
        resolvedDependencyLocation = ResolvedDependencyLocation([self.dependency])
        navResult = NavigationResult(resolvedDependencyLocation, self.dependency)
        arrayNR = AstNavigationResult(navResult, self.isLHSType)

        # We need to modify the last relative location so copy the path.
        copiedPath = Location([RelativeLocation(p.kind, p.name, templateParameters=p.templateParameters, parameters=p.parameters, dims=p.dims) for p in self.dependency.location])
        arrayNR.explicitLocation = copiedPath
        arrayNR.explicitLocation[-1].dims = dims

        return arrayNR

    def as_base(self):
        """
        Return the navigation result as a base type without array dimensions.

        This function returns a navigation result with the same dependency, but the explicit location
        will have the array dimensions unset.

        Returns:
            linker.AstNavigationResult: The same result as a base type.
        """
        return self.as_array([])

    def __eq__(self, other):
        """
        Return whether two navigation results point to the same location.

        Args:
            other (linker.NavigationResult): The navigation result to compare with.
        Returns:
            bool: True, if both results point to the same location. Otherwise False.
        """
        return self.explicitLocation == other.explicitLocation

class LinkableProject:
    """
    A project which can be linked.

    Attributes:
        unresolvedDependencies ({linker.Dependency}): A set of unresolved dependencies.
        unresolvedAliasDefaultConstructorDependencies ({linker.Dependency}): A set of unresolved alias constructor dependencies.
        libraries (dict): The libraries lookup table.
        resolvedObjects (defaultdict): Maps each library to a list of resolved objects
        links (dict): Maps unresolved dependencies to their resolved counterparts
        locationConflicts ([objects.LocationConflict]): A list of location conflicts.
    """

    def __init__(self, userTypeLocationStrings):
        """
        Initialize the object.

        Args:
            userTypeLocationStrings (dict): The user-defined system type location strings from the project configuration.
        """
        self.unresolvedDependencies = list()
        self.unresolvedAliasDefaultConstructorDependencies = list()
        self.libraries = {} # The libraries lookup table
        self.libRoots = {} # The library root namespace lookup table
        self.libNamespaces = dict() # Maps namespace locations to namespaces
        self.resolvedObjects = defaultdict(ResolvedDependencyLocation) # Maps each library to a set of resolved objects
        self.links = {} # Maps unresolved dependencies to their resolved counterparts.
        self.locationConflicts = [] # Locations that point to the same endpoint (conflicts).
        self.libName = None # The name of the library that is being monitored.
        self.templateLinks = {} # Maps locations to template instantiation results.
        self.functions = [] # An internal cache of function objects inside the current library.
        self.properties = [] # An internal cache of properties objects inside the current library.
        self.preImports = [] # The pre-imports to use.
        self.postImports = [] # The post-imports to use.
        self._libLocationNavResults = {} # Maps navigation queries to their resolved navigation result.

        # Cache the user-specified locations for numeric types.
        lexer = SymbolicLexer(libName=None, fileName=None)

        # Maps native types to user-specified typename locations
        validUserTypeLocationStrings = { k: str(v) for k, v in userTypeLocationStrings.items() if k in Language.systemTypenames }
        self.nativeTypenameLocations = { k: None for k in Language.systemTypenames }

        for nativeTypename, userLocationString in validUserTypeLocationStrings.items():
            lexer.fileName = "$SystemTypes/{0}".format(nativeTypename)

            # Convert the user location string to a token stream.
            userTypeLocationTokens = lexer.tokenize(userLocationString)

            # Setup a parser to analyze that token stream.
            parser = UnitParser(lexer.libName, lexer.fileName, userTypeLocationTokens, self.preImports, self.postImports)

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
            ExpressionAtomKind.String: self._verify_ast_string,
            ExpressionAtomKind.FunctionBegin: self._verify_ast_function,
            ExpressionAtomKind.ArrayBegin: self._verify_ast_array,
            ExpressionAtomKind.TemplateBegin: self._verify_ast_template,
            ExpressionAtomKind.UnaryOp: self._verify_ast_unary_op,
            ExpressionAtomKind.BinaryOp: self._verify_ast_binary_op
        }

    def _ast_try_navigate_dependency(self, locatable, isLHSType):
        """
        Try to navigate to a locatable object.

        Args:
            locatable (objects.Locatable): The locatable object to search for.
        Returns:
            linker.AstNavigationResult or None: The navigation result or None.
        """
        dependency = Dependency(locatable)
        navResult = self.try_navigate_dependency(dependency)
        if navResult is None or navResult.dependency is None:
            return None

        astNavResult = AstNavigationResult(navResult, isLHSType)

        # Navigate through aliases down to base.
        if astNavResult.explicitLocation.path[-1].kind == LocationKind.Type:
            navResult = self.navigate_alias_base(navResult)
            astNavResult = AstNavigationResult(navResult, isLHSType)

        # Copy array dimensions, if not scalar.
        if isinstance(locatable, Typename) and locatable.dims:
            if astNavResult.explicitLocation.path[-1].dims:
                raise InvalidAliasDimensionsError(locatable.anchor)

            astNavResult = astNavResult.as_array(locatable.dims)

        return astNavResult

    def _ast_navigate_dependency(self, locatable, isLHSType):
        """
        Navigate to a locatable object.

        Args:
            locatable (objects.Locatable): The locatable object to search for.
            isLHSType (bool): True, if this is a LHS type.
        Returns:
            linker.AstNavigationResult: The navigation result.
        """
        navResult = self._ast_try_navigate_dependency(locatable, isLHSType)
        if navResult is None:
            raise DependencyNotFoundError(locatable.anchor, locatable.location())

        return navResult

    def _try_verify_ast_member_or_property(self, container, name, lhs, isAssignment):
        """
        Verify a struct member or property in an AST.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            name (lexer.Symto): The member or property name.
            lhs (linker.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
            isAssignment (bool): True, if this is an assignment. Otherwise False.
        Returns:
            linker.AstNavigationResult or None: The location of the resulting type of this AST.
        """
        if lhs is None:
            if not isinstance(container, Property):
                return None

            # Container is implicit LHS.
            struct = container.parent
            assert(isinstance(struct, Struct))

            isLHSType = False
        else:
            # Deduce struct from LHS if possible.
            struct = lhs.dependency.locatable
            if not isinstance(struct, Struct):
                return None

            isLHSType = lhs.isLHSType

        result = self._try_verify_ast_member(struct, name)
        result = None if isLHSType and not Annotation.has(Language.static, struct.annotations) else result
        result = self._try_verify_ast_property(struct, name, [], isLHSType, isAssignment) if result is None else result

        return result

    def _try_verify_ast_property(self, struct, name, parameters, isStatic, isAssignment):
        """
        Verify a property in an AST.

        Args:
            struct (objects.Struct): The structure.
            name (lexer.Symto): The property name token.
            parameters ([objects.Parameter]): The parameter list.
            isStatic (bool): True, if this is a static property. Otherwise False.
            isAssignment (bool): True, if this is an assignment. Otherwise False.
        Returns:
            linker.AstNavigationResult or None: The location of the resulting type of this AST.
        """
        propertyParameters = list(parameters)

        if not isStatic:
            propertyParameters.insert(0, Parameter.this_parameter(struct.references, struct.location()))

        propertyNR = self._try_find_property(struct, name, propertyParameters)
        if propertyNR is not None:
            func = propertyNR.dependency.locatable
            if isAssignment and not func.hasSet:
                raise SetNotSupportedError(name.anchor)

            # Lookup the return type.
            funcRetTypenameNR = self._ast_navigate_dependency(func.returnTypename, False)
            return funcRetTypenameNR

        return None

    def _try_verify_ast_member(self, struct, memberName):
        """
        Verify a member in an AST.

        Args:
            struct (objects.Struct): The structure.
            memberName (lexer.Symto): The member name.
        Returns:
            linker.AstNavigationResult or None: The location of the resulting type of this AST.
        """
        memberTypename = struct.try_find_member_typename(memberName.anchor, memberName)
        if memberTypename is None:
            return None

        return self._ast_try_navigate_dependency(memberTypename, False)

    def _try_verify_ast_namespace_object(self, atom, lhs):
        """
        Verify a namespace object in an AST.

        Args:
            atom (objects.ExpressionAtom): The root atom.
            lhs (linker.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
        Returns:
            linker.AstNavigationResult or None: The location of the resulting type of this AST.
        """
        if lhs is None:
            return None

        namespace = lhs.dependency.locatable
        if not isinstance(namespace, Namespace):
            return None

        for loc in namespace.locatables:
            if isinstance(loc, Namespace) and loc.token == atom.token:
                libName = str(lhs.explicitLocation[0])
                loc.token.anchor.libName = libName
                objNR = self._ast_try_navigate_dependency(loc, True)
                return objNR

        return None

    def _verify_ast_var(self, container, atom, children, localVars, newLocalVars, isOptional, lhs, isAssignment):
        """
        Verify an AST with a variable atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (linker.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
            isAssignment (bool): True, if this is an assignment. Otherwise False.
        Returns:
            linker.AstNavigationResult: The location of the resulting type of this AST.
        """
        if lhs is None:
            # Handle boolean values: true, false.
            if atom.token in ["true", "false"]:
                boolTypename = self.native_bool_typename(container.references)
                navResult = self._ast_navigate_dependency(boolTypename, False)
                return navResult

            # Try local variables next.
            varNR = localVars.get(str(atom.token), None)
            if varNR is not None:
                return varNR

        # Try dependent lookup as struct member.
        memberNR = self._try_verify_ast_member_or_property(container, atom.token, lhs, isAssignment)
        if memberNR is not None:
            return memberNR

        # Try dependent lookup as namespace object.
        namespaceObjNR = self._try_verify_ast_namespace_object(atom, lhs)
        if namespaceObjNR is not None:
            return namespaceObjNR

        # Try as object lookup, climbing up the hierarchy.
        # Could be a namespace, struct or alias.
        navResult = self._ast_try_navigate_any([LocationKind.Namespace, LocationKind.Type], atom.token, container.references)
        if navResult is not None:
            return navResult

        if not isOptional:
            raise UnresolvedSymbolError(atom.token)

        return None

    def _ast_try_navigate_any(self, locationKinds, token, references):
        """
        Try to navigate any of the specified location types.

        Args:
            locationKinds ([objects.LocationKind]): The location kinds to navigate.
            token (lexer.Symto): The name to navigate.
            references ([objects.Reference]): The references.
        """
        for locationKind in locationKinds:
            location = RelativeLocation(locationKind, token).location()
            typename = Typename.from_location(references, location)
            navResult = self._ast_try_navigate_dependency(typename, True)
            if navResult is not None and navResult.explicitLocation[-1].kind == locationKind:
                return navResult

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
            lhs (linker.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
        Returns:
            linker.AstNavigationResult: The location of the resulting type of this AST.
        """
        return self._ast_try_navigate_any([LocationKind.Type], token, references)

    def _verify_ast_number(self, container, atom, children, localVars, newLocalVars, isOptional, lhs, isAssignment):
        """
        Verify an AST with a number atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (linker.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
            isAssignment (bool): True, if this is an assignment. Otherwise False.
        Returns:
            linker.AstNavigationResult: The location of the resulting type of this AST.
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

        result = self._ast_navigate_dependency(typename, False)
        return result

    def _verify_ast_string(self, container, atom, children, localVars, newLocalVars, isOptional, lhs, isAssignment):
        """
        Verify an AST with a string atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (linker.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
            isAssignment (bool): True, if this is an assignment. Otherwise False.
        Returns:
            linker.AstNavigationResult: The location of the resulting type of this AST.
        """
        if atom.token is None or atom.token.isString:
            # String
            typename = self.native_string_typename(container.references)

        result = self._ast_navigate_dependency(typename, False)
        return result

    def _verify_ast_function(self, container, atom, children, localVars, newLocalVars, isOptional, lhs, isAssignment):
        """
        Verify an AST with a function atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (linker.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
            isAssignment (bool): True, if this is an assignment. Otherwise False.
        Returns:
            linker.AstNavigationResult: The location of the resulting type of this AST.
        """
        # Extract child information.
        childNRs = [self._verify_expression_ast_recursive(container, localVars, child, newLocalVars) for child in children]
        childTypenames = [Typename.from_location(container.references, childNR.explicitLocation) for childNR in childNRs]
        parameters = [Parameter(container, child.atom.token, [], None, childTypenames[i], child.isRef) for i, child in enumerate(children)]

        isExplicitRef = lhs is not None
        parent = lhs.dependency.locatable if isExplicitRef else container
        if isExplicitRef and isinstance(lhs.dependency.locatable, Struct):
            result = self._try_verify_ast_property(parent, atom.token, parameters, lhs.isLHSType, isAssignment)
            if result is not None:
                return result
        else:
            possibleMatchNR = self._try_find_function(parent, atom.token, FunctionKind.Operator if atom.token.isOp else FunctionKind.Regular, parameters, isExplicitRef=isExplicitRef)
            if possibleMatchNR is not None:
                result = self._ast_navigate_dependency(possibleMatchNR.dependency.locatable.returnTypename, False)
                return result

        raise FunctionOverloadNotFoundError(atom.token, parameters)

    def _verify_ast_array(self, container, atom, children, localVars, newLocalVars, isOptional, lhs, isAssignment):
        """
        Verify an AST with an array atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (linker.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
            isAssignment (bool): True, if this is an assignment. Otherwise False.
        Returns:
            linker.AstNavigationResult: The location of the resulting type of this AST.
        """
        # Extract child information.
        isScalar = len(children) > 0
        for child in children:
            childNR = self._verify_expression_ast_recursive(container, localVars, child, newLocalVars)
            childTypename = Typename.from_location(container.references, childNR.explicitLocation)

            # Make sure the child (here: array index) resolves to an integer.
            intTypename = self.native_int_typename(container.references)
            intNR = self._ast_navigate_dependency(intTypename, False)

            if childNR != intNR:
                raise InvalidArrayIndexTypeError(atom.token.anchor)

            if child.atom.token != "1":
                isScalar = False

        varNR = self._verify_ast_var(container, atom, children, localVars, newLocalVars, True, lhs, isAssignment)
        if varNR:
            # Make sure the indexing matches the type dimensions.
            varTypeDims = varNR.explicitLocation[-1].dims
            if len(varTypeDims) > 0: # False positive
                expectedDims = max(1, len(varTypeDims))
                if len(children) != expectedDims:
                    raise InvalidArrayIndexDimensionsError(atom.token.anchor, expectedDims)

                # Remove array bounds.
                baseTypeNR = varNR.as_base()
                return baseTypeNR

        memberNR = self._try_verify_ast_member_or_property(container, atom.token, lhs, isAssignment)
        if memberNR:
            return memberNR

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
        loc = self.nativeTypenameLocations[name]
        if loc is None:
            raise UnmappedNativeTypename(name)

        return Typename.from_location(references, loc)

    def native_int_typename(self, references):
        """
        Return the native int typename.

        Args:
            references (list): The references to associate with the typename.
        Returns:
            Typename: The typename.
        """
        return self.native_typename(references, "int")

    def native_void_typename(self, references):
        """
        Return the native void typename.

        Args:
            references (list): The references to associate with the typename.
        Returns:
            Typename: The typename.
        """
        return self.native_typename(references, "void")

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

    def native_string_typename(self, references):
        """
        Return the native string typename.

        Args:
            references (list): The references to associate with the typename.
        Returns:
            Typename: The typename.
        """
        return self.native_typename(references, "string")

    def _verify_ast_template(self, container, atom, children, localVars, newLocalVars, isOptional, lhs, isAssignment):
        """
        Verify an AST with a template atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (linker.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
            isAssignment (bool): True, if this is an assignment. Otherwise False.
        Returns:
            linker.AstNavigationResult: The location of the resulting type of this AST.
        """
        # Template function calls are not supported yet.
        assert False, "Template functions are not yet supported."

    def _verify_ast_unary_op(self, container, atom, children, localVars, newLocalVars, isOptional, lhs, isAssignment):
        """
        Verifies an AST with a unary operator atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (linker.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
            isAssignment (bool): True, if this is an assignment. Otherwise False.
        Returns:
            linker.AstNavigationResult: The location of the resulting type of this AST.
        """
        assert(len(children) == 1)

        # Extract child information.
        child = children[0]
        childNR = self._verify_expression_ast_recursive(container, localVars, child, newLocalVars)
        childTypename = Typename.from_location(container.references, childNR.explicitLocation)

        # Lookup the operator.
        childParameter = Parameter(container, child.atom.token, [], None, childTypename, True)
        possibleMatchNR = self._try_find_function(container, atom.token, FunctionKind.Operator, [childParameter])

        if possibleMatchNR is None:
            # Try it again with non-ref versions.
            childParameter.isRef = not childParameter.isRef
            possibleMatchNR = self._try_find_function(container, atom.token, FunctionKind.Operator, [childParameter])

        if possibleMatchNR is not None:
            # Lookup the return type.
            funcRetTypenameNR = self._ast_navigate_dependency(possibleMatchNR.dependency.locatable.returnTypename, False)
            return funcRetTypenameNR

        raise UnaryOperatorOverloadNotFoundError(atom.token, childTypename)

    def _try_find_function(self, container, nameToken, kind, parameters, *, templateParameters=None, isExplicitRef=False):
        """
        Try to find a function matching a given signature.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            nameToken (lexer.Symto): A token with the name of the function to look for.
            kind (objects.FunctionKind): The function kind to look for.
            parameters (list of objects.Parameter): The parameter signature.
        Returns:
            linker.AstNavigationResult: The location of the resulting type of this AST.
        """
        locatable = FunctionReference(container.references, container, nameToken, kind, templateParameters, parameters, isExplicitRef)
        navResult = self._ast_try_navigate_dependency(locatable, False)
        return navResult

    def _try_find_property(self, container, nameToken, parameters, *, isExplicitRef=False):
        """
        Try to find a function matching a given signature.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            nameToken (lexer.Symto): A token with the name of the function to look for.
            parameters (list of objects.Parameter): The parameter signature.
        Returns:
            linker.AstNavigationResult: The location of the resulting type of this AST.
        """
        locatable = PropertyReference(container.references, container, nameToken, parameters, isExplicitRef)
        navResult = self._ast_try_navigate_dependency(locatable, False)
        return navResult

    def _verify_ast_binary_op(self, container, atom, children, localVars, newLocalVars, isOptional, lhs, isAssignment):
        """
        Verify an AST with a binary operator atom type.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars (dict): Lookup table that maps new variable names to resolved locations.
            isOptional (bool): State flag which indicates that the query should not throw an error on failure.
            lhs (linker.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
            isAssignment (bool): True, if this is an assignment. Otherwise False.
        Returns:
            linker.AstNavigationResult: The location of the resulting type of this AST.
        """
        assert(len(children) == 2)

        left, right = children[0], children[1]

        isNewVarOp = atom.token == ":="
        isStructOp = atom.token == "."
        isAssignOp = atom.token == "="
        leftNR = None

        # Try to deduce the LHS.
        if isStructOp and lhs is None:
            inOrderTokens = []
            node = atom
            nodeChildren = children
            nodeLhs = None
            nodeRhs = None
            leftNR = None
            while node.token == ".":
                nodeLhs = nodeChildren[0]
                nodeRhs = nodeChildren[1]

                inOrderTokens.append(nodeLhs.atom.token)
                node = nodeRhs.atom
                nodeChildren = nodeRhs.children

            # Match longest library name and figure out which namespace to navigate to in the library.
            matchedLibName = None
            for i in reversed(range(1, len(inOrderTokens)+1)):
                potentialLib = inOrderTokens[:i]
                potentialLibName = Algorithm.join_dot(potentialLib)
                if potentialLibName == self.libName or container.has_reference(potentialLibName):
                    # Match found.
                    # Skip the library nodes.
                    matchedLibName = potentialLibName
                    break

            if matchedLibName is not None:
                # Update current position in tree.
                left = nodeLhs
                right = nodeRhs
                children = nodeChildren

                # Update LHS using library root.
                libRoot = self.libRoots[matchedLibName]
                lhs = self._ast_navigate_dependency(libRoot, True)

                if len(inOrderTokens) == 1:
                    leftNR = lhs

        if leftNR is None:
            leftNR = self._verify_expression_ast_recursive(container, localVars, left, newLocalVars, isOptional=isNewVarOp, lhs=lhs, isAssignment=isAssignOp)
        leftResolved = leftNR if isStructOp else None
        if leftResolved is not None and leftResolved.explicitLocation[-1].dims:
            raise MissingArrayDimensionsError(left.atom.token.anchor)

        rightNR = self._verify_expression_ast_recursive(container, localVars, right, newLocalVars, lhs=leftResolved, isAssignment=(isAssignment and not isAssignOp))

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
            pLeft = Parameter(container, left.atom.token, [], None, leftTypename, isAssignOp)
            pRight = Parameter(container, right.atom.token, [], None, rightTypename, False)

            # Try to find a match for the signature.
            possibleMatchNR = self._try_find_function(container.parent, atom.token, FunctionKind.Operator, [pLeft, pRight])
            if possibleMatchNR is None:
                # Try it again with non-ref versions.
                pLeft.isRef = not pLeft.isRef
                possibleMatchNR = self._try_find_function(container.parent, atom.token, FunctionKind.Operator, [pLeft, pRight])
                if possibleMatchNR is None:
                    pRight.isRef = not pRight.isRef
                    possibleMatchNR = self._try_find_function(container.parent, atom.token, FunctionKind.Operator, [pLeft, pRight])
                    if possibleMatchNR is None:
                        pLeft.isRef = not pLeft.isRef
                        possibleMatchNR = self._try_find_function(container.parent, atom.token, FunctionKind.Operator, [pLeft, pRight])

            if possibleMatchNR is not None:
                # Lookup the return type.
                funcRetTypenameNR = self._ast_navigate_dependency(possibleMatchNR.dependency.locatable.returnTypename, False)
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
            linker.NavigationResult or None: The resolved location or None, if no match was found.
        """
        # Lookup from ID cache.
        navQuery = NavigationQuery(references, parent, location)
        navQueryStr = str(navQuery)
        result = self._libLocationNavResults.get(navQueryStr, None)
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

        # Generate a sequence of additional locations to search at.
        # This will search the parent location, and all of its parents first.
        isPropertyLocation = location[-1].kind == LocationKind.Property
        locationsWithoutLibName = deque()
        currentParent = parent
        while currentParent is not None and currentParent.parent is not None:
            # Prepend the location of the parent to the search path.
            parentPath = currentParent.location().pathWithoutRef
            otherLocationWithoutLibName = Location(parentPath + locationWithoutLibName)
            locationsWithoutLibName.append(otherLocationWithoutLibName)
            currentParent = currentParent.parent

            if isPropertyLocation:
                break

        if not isPropertyLocation:
            locationsWithoutLibName.append(Location(locationWithoutLibName))

        # Find the library and the object in the library
        lookup = None
        resolvedLibName = None
        locationFound = False
        matchedDependency = None
        discardParentLocations = False
        for libName in libNameGen:
            if not discardParentLocations:
                discardParentLocations = not isPropertyLocation
            else:
                locationsWithoutLibName = [locationWithoutLibName]

            # Assume this library contains the dependency
            resolvedLibName = libName
            baseLookup = self.resolvedObjects[libName]
            baseLookup = next(iter(baseLookup.subLocations.values())) if baseLookup.subLocations else baseLookup
            lookup = baseLookup

            # Assume we match the root lib namespace.
            matchedDependency = Dependency(self.libRoots[libName])

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

                    # Determine possible matches.
                    possibleMatches = filter(lambda dependency: i < len(dependency.baseLocationWithoutRef) and dependency.baseLocationWithoutRef[i].can_be_resolved_to(rl), dependencies)
                    if not possibleMatches:
                        continue

                    # Find the best match out of the possible matches by counting the number of
                    # partial matches. The highest match count wins.
                    bestMatches = sorted(possibleMatches, reverse=True, key=lambda e: e.baseLocationWithoutRef[i].numPartialMatches)

                    for dependency in bestMatches:
                        dependencyRL = dependency.baseLocationWithoutRef[i]
                        dependencyLocatable = dependency.locatable

                        if isinstance(dependencyLocatable, (Function, FunctionReference, Property, PropertyReference)):
                            # Validate requested and found signature.
                            requestedParams = rl.parameters
                            foundParams = dependencyRL.parameters

                            # Both lists are guaranteed to be the same length here.
                            # We just need to check the target locations.
                            assert(len(requestedParams) == len(foundParams))
                            parameterMismatch = False
                            for paramIndex, paramA in enumerate(requestedParams):
                                paramB = foundParams[paramIndex]

                                assert(paramA.isRef == paramB.isRef)
                                assert(paramA.typename.dims == paramB.typename.dims)

                                paramTypenameA = paramA.typename
                                paramTypenameB = paramB.typename

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

                            matchedDependency = dependency
                        elif isinstance(dependencyLocatable, Alias):
                            # If it is an alias then we have to lookup the aliased type instead
                            # since aliases don't have any sublocations.
                            # But only do that if this is not the last relative location
                            # since we are actually looking for the Alias in that case.
                            if i != len(location.pathWithoutRef) - 1:
                                aliasNavResult = NavigationResult(resolvedDependencyLocation, dependency)
                                aliasNavResult = self.navigate_alias_base(aliasNavResult)
                                libName = aliasNavResult.dependency.libName
                                matchedDependency = aliasNavResult.dependency
                                resolvedDependencyLocation = aliasNavResult.resolvedDependencyLocation
                            else:
                                matchedDependency = dependency
                        elif isinstance(dependencyLocatable, Template):
                            try:
                                navResult = self.instantiate_template(dependencyLocatable, dependencyRL.templateParameters, rl.templateParameters, references)
                            except RecursionError:
                                raise MaxTemplateRecursionError(dependencyLocatable.token.anchor)

                            resolvedDependencyLocation = navResult.resolvedDependencyLocation
                            matchedDependency = navResult.dependency
                        else:
                            matchedDependency = dependency

                        # Step down this namespace
                        allSubLocationsFound = True
                        lookup = resolvedDependencyLocation
                        break

                if allSubLocationsFound:
                    break

            # If we verified the location we can stop the search.
            locationFound = allSubLocationsFound
            if locationFound:
                break

        if not locationFound:
            return None

        result = NavigationResult(lookup, matchedDependency)

        # Cache the result.
        # This is used to lookup function parameter typename locations later.
        self._libLocationNavResults[navQueryStr] = result

        return result

    def template_instance_str(self, templateParameters, references, baseLocation):
        """
        Return a string that describes a template instance.

        Args:
            cls: Unused.
            templateParameters ([objects.TemplateParameter]): The template parameters.
            references ([objects.Reference]): The library references.
            baseLocation (objects.Location): The base location of the template.
        Returns:
            str: A dependency location string for the template. This uniquely identifies the template-parameter combination.
        """
        templateInstanceArgs =  ", ".join(str(p) for p in templateParameters)
        importLibs = ", ".join(str(ref) for ref in references)
        importLibs = " using {0}".format(importLibs) if importLibs else importLibs
        dependencyLocationStr = "{0} with <{1}>{2}".format(str(baseLocation), templateInstanceArgs, importLibs)
        return dependencyLocationStr

    def navigate_to_template_object(self, dependencyLocationStr):
        """
        Navigate to an instantiated template using its instance string.

        Args:
            dependencyLocationStr (str): The template instantiation string.
        Returns:
            linker.NavigationResult: The navigation result.
        """
        # Use template links to jump to the right location, which is anonymous.
        templateDep = Dependency(self.templateLinks[dependencyLocationStr])
        templateNavResult = self.navigate_dependency(templateDep)
        templateAliasNavResult = self.navigate_alias_base(templateNavResult)
        return templateAliasNavResult

    def instantiate_template(self, template, templateParameterNames, templateParameterValues, references):
        """
        Instantiate a template with given parameters.

        Args:
            template (objects.Template): The template to instantiate.
            templateParameterNames ([objects.TemplateParameter]): The template parameter names, e.g. T0.
            templateParameterValues ([objects.TemplateParameter]): The template parameter values, e.g. "int".
            references ([objects.Reference]): The references for the template.
        Returns:
            linker.NavigationResult: The navigation result.
        """
        dependencyLocationStr = self.template_instance_str(templateParameterValues, references, template.location().base())

        if dependencyLocationStr not in self.templateLinks:
            # Generate the translation unit for the template.
            templateSrc = template.generate_translation_unit(references)

            # Run the pre-processor on the source.
            # TODO: run pre-processor based on the location
            ppTemplateSrc = templateSrc # self.preprocessor.run(templateSrc, LIBNAME, PPT)

            # Lex the unit so we can parse it.
            lexer = SymbolicLexer(libName=self.libName, fileName="$Templates/{0}".format(str(template.token)))

            # Plugin the template substitutions for the lexer.
            templateSubs = { str(templateParameterNames[i]): str(templateParameterValue)[1:-1] for i, templateParameterValue in enumerate(templateParameterValues) }

            # Generate a parsable token stream now.
            srcFileTokens = lexer.tokenize(ppTemplateSrc, subs=templateSubs)
            srcFileTokens = lexer.concatenate_tokens(srcFileTokens)

            # Analyze the token stream.
            parser = UnitParser(lexer.libName, lexer.fileName, srcFileTokens, self.preImports, self.postImports)
            rootNamespace = parser.parse()

            # Lookup the template object
            templateObj = rootNamespace
            while type(templateObj) is Namespace:
                templateObj = templateObj.locatables[0]

            # Bind the location to a template.
            self.templateLinks[dependencyLocationStr] = templateObj

            # Insert it into the collection so we can look it up.
            # Update the references to match the call site.
            rootNamespace.references = references
            self.insert_unit(rootNamespace)

        result = self.navigate_to_template_object(dependencyLocationStr)
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
            linker.NavigationResult: The resolved location.
        """
        navResult = self.try_navigate(errorAnchor, references, parent, location)
        if navResult is None:
            raise DependencyNotFoundError(errorAnchor, location)
        return navResult

    def navigate_parent(self, dependency):
        """
        Navigate to the parent of a dependency.

        Args:
            dependency (linker.Dependency): The dependency to find the parent of.
        Returns:
            linker.NavigationResult: The navigation result.
        """
        locatable = dependency.locatable
        location = dependency.baseLocation
        return self.navigate(locatable.anchor, locatable.references, locatable.parent.parent if locatable.parent is not None else None, location[:-1])

    def try_merge_namespace(self, locatable):
        """
        Try to merge a namespace with existing namespaces.

        Args:
            locatable (objects.Locatable): The namespace to merge.
        Returns:
            objects.Locatable: The merged namespace or the identity transform, if the locatable is not a namespace.
        """
        if type(locatable) is not Namespace:
            return False

        namespaceLoc = str(locatable.location())
        namespace = self.libNamespaces.get(namespaceLoc, None)
        if namespace is None:
            self.libNamespaces[namespaceLoc] = locatable
            return False

        # Re-parent children.
        namespace.merge(locatable)

        return True

    def insert(self, locatable):
        """
        Insert an object into the dependency collection.

        Args:
            locatable (objects.Locatable): The object to insert.
        """
        if isinstance(locatable, Instruction):
            return

        # Merge namespaces
        if self.try_merge_namespace(locatable):
            return

        # Recurse on the locatables dependencies.
        if isinstance(locatable, Function):
            # The unknown return type
            self.insert(locatable.returnTypename)

            # The unknown parameter types
            for parameter in locatable.parameters:
                self.insert(parameter.typename)

            # Remember this function for instruction verification later.
            self.functions.append(locatable)
        elif isinstance(locatable, Property):
            # The unknown return type
            self.insert(locatable.returnTypename)

            # The unknown parameter types
            for parameter in locatable.parameters:
                self.insert(parameter.typename)

            # Remember this property for instruction verification later.
            self.properties.append(locatable)
        elif isinstance(locatable, Alias):
            # The unknown aliased typename.
            self.insert(locatable.targetTypename)
        elif isinstance(locatable, MemberList):
            # Unknown member types.
            self.insert(locatable.typename)

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

                    # If the template and signature matches then it might be a conflict
                    if rl.can_be_resolved_to(otherDependencyRL):
                        # Partial matches must match exactly.
                        isExactPartialMatch = Algorithm.zip_all(rl.templateParameters, otherDependencyRL.templateParameters,
                                                                lambda p0, p1: p0.partialMatch == p1.partialMatch)
                        if isExactPartialMatch:
                            conflict = LocationConflict(dependency, otherDependency)
                            self.locationConflicts.append(conflict)

                # Register dependency at this location
                existingDependencies.append(dependency)
            else:
                lookup[rl.name] = ResolvedDependencyLocation([dependency])

            # If it's a type generate the default constructors.
            if isinstance(locatable, Struct):
                self._generate_type_constructor(locatable, locatable)
                self._generate_assignment_op(locatable)
            elif isinstance(locatable, Alias):
                self.unresolvedAliasDefaultConstructorDependencies.append(dependency)

    def _generate_alias_constructor(self, dependency):
        """
        Generate the default constructor for an alias.

        Args:
            dependency (linker.Dependency): The alias dependency.
        """
        nr = self.navigate_dependency(dependency)
        structNR = self.navigate_alias_base(nr)
        struct = structNR.dependency.locatable
        self._generate_type_constructor(dependency.locatable, struct)

    def insert_unit(self, rootNamespace):
        """
        Insert an unresolved translation unit into the collection.

        Args:
            rootNamespace (objects.Namespace): The global namespace.
        """
        # Initialize library root namespace.
        if self.libName not in self.libRoots:
            self.libRoots[self.libName] = rootNamespace

        locatables = deque()
        locatables.append(rootNamespace)
        while locatables:
            locatable = locatables.popleft()

            self.insert(locatable)

            if isinstance(locatable, Namespace):
                locatables += locatable.locatables

    def begin_library(self, libName, preImports, postImports):
        """
        Begin collecting dependencies for a new library.

        Args:
            libName (str): The name of the library.
            preImports (list): The pre-imports to use.
            postImports (list): The post-imports to use.
        """
        assert(self.libName is None)

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

        self.libNamespaces = dict()
        self.libName = libName
        self.functions = []
        self.properties = []
        self.preImports = preImports
        self.postImports = postImports
        self._libLocationNavResults = {}

    def end_library(self):
        """End collecting dependencies for the current library."""
        self._resolve()

        # Close this library
        self.libName = None
        self.functions = []
        self.properties = []

    def _generate_type_constructor(self, locatable, struct):
        """
        Generate a type constructor for a given type.

        Args:
            locatable (objects.Locatable): The locatable to generate the constructor for.
            struct (objects.Struct): The type to generate the constructor for.
        """
        if Annotation.has(Language.noConstructor, locatable.annotations) or Annotation.has(Language.static, locatable.annotations):
            return

        # The return type is the struct.
        returnTypename = Typename.from_location(locatable.references, struct.location())

        # The parameters are deduced from the member lists.
        parameters = []
        for memberList in struct.locatables:
            if isinstance(memberList, MemberList):
                if not Annotation.has("uninitialized", memberList.annotations):
                    parameters += [Parameter(None, member.token, [], None, memberList.typename, False) for member in memberList]

        constructor = Function(locatable.references, locatable.parent, locatable.token, [], None, FunctionKind.Regular, returnTypename, parameters)
        self.insert(constructor)

    def _generate_assignment_op(self, struct):
        """
        Generate an assignment operator for a given type.

        Args:
            struct (objects.Struct): The struct to generate the operator for.
        """
        if Annotation.has(Language.noAssignment, struct.annotations):
            return

        typename = Typename.from_location(struct.references, struct.location())
        unnamed = Symto.from_token(struct.token, Token.Name, "")
        lhs = Parameter(None, unnamed, [], None, typename, True)
        rhs = Parameter(None, unnamed, [], None, typename, False)
        parameters = [lhs, rhs]
        opName = Symto.from_token(struct.token, Token.Operator, "=")
        op = Function(struct.references, struct.parent, opName, [], None, FunctionKind.Operator, typename, parameters)
        self.insert(op)

    def _resolve_pending_dependencies(self, unresolvedDependencies):
        """Resolve all dependencies."""
        # Traverse from top to bottom
        localUnresolvedDependencies = deque(unresolvedDependencies)

        # Clear the unresolved dependencies, they are in the queue
        unresolvedDependencies.clear()

        while localUnresolvedDependencies:
            dependency = localUnresolvedDependencies.popleft()

            # The name has to be resolvable by now
            # Catching unresolved objects will spawn templates, if encountered
            self.links[dependency] = self.navigate_dependency(dependency)

            # Add new unresolved dependencies to the queue
            localUnresolvedDependencies += unresolvedDependencies

            # And clear the cache again
            unresolvedDependencies.clear()

    def _resolve(self):
        """Resolve all dependencies."""
        self._resolve_pending_dependencies(self.unresolvedDependencies)

        # Now that each dependency has been resolved we can generate the alias default constructor which require the base type to be resolvable.
        for dependency in self.unresolvedAliasDefaultConstructorDependencies:
            self._generate_alias_constructor(dependency)

        self.unresolvedAliasDefaultConstructorDependencies.clear()

        # We solve location conflicts due to clashing parameter signatures by comparing the types now.
        self._solve_location_conflicts()

        # Now that all members and function parameters are resolved we can look at the instructions within the functions.
        self._verify_functions()
        self._verify_properties()

    def _verify_functions(self):
        """Verify all instructions within all instantiated functions of the current library."""
        for func in self.functions:
            self._verify_function(func)

    def _verify_properties(self):
        """Verify all instructions within all instantiated properties of the current library."""
        for prop in self.properties:
            self._verify_property(prop)

    def _local_vars_from_parameters(self, parameters):
        """
        Deduce the local parameter list from a parameter list.

        Args:
            parameter ([objects.Parameter]): The parameter list.
        Returns:
            dict: The local variable dictionary.
        """
        return { str(p.token): self._ast_navigate_dependency(p.typename, False) for p in parameters }

    def _verify_function(self, func):
        """
        Verify the contents of a given function body.

        Args:
            func (objects.Function): The function object.
        """
        localVars = self._local_vars_from_parameters(func.parameters)
        returnTypeNR = self._ast_navigate_dependency(func.returnTypename, False)
        self._verify_instructions(func, localVars, func.instructions, ScopeState.Default, returnTypeNR)

    def _verify_property(self, prop):
        """
        Verify the contents of a given property body.

        Args:
            prop (objects.Property): The property object.
        """
        localVars = self._local_vars_from_parameters(prop.parameters)
        returnTypeNR = self._ast_navigate_dependency(prop.returnTypename, False)
        self._verify_instructions(prop, localVars, prop.getInstructions, ScopeState.Default, returnTypeNR)

        if prop.hasSet:
            localVars[Language.value] = self._ast_navigate_dependency(prop.returnTypename, False)
            self._verify_instructions(prop, localVars, prop.setInstructions, ScopeState.Default, returnTypeNR)

    def _verify_instruction(self, container, localVars, instruction, scopeState, prevInstruction, returnTypeNR):
        """
        Verify an instruction.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            localVars (dict): The local variables.
            instruction (objects.Instruction): The instruction to verify.
            scopeState (linker.ScopeState): The current scope state.
            prevInstruction (objects.Instruction): The previous instruction in the same scope.
            returnTypeNR (linker.AstNavigationResult): The return type AST navigation result.
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
            if instruction.expression is None:
                exprType = self.native_void_typename(container.references)
                exprTypeNR = self._ast_navigate_dependency(exprType, False)
            else:
                exprTypeNR = self._verify_expression_ast(container, localVars, instruction.expression.ast)

            # Make sure the expression type matches the return type.
            if returnTypeNR != exprTypeNR:
                raise ReturnTypeMismatchError(instruction.anchor)
        elif instruction.kind in [InstructionKind.If, InstructionKind.While, InstructionKind.Elif]:
            # Verify instruction.
            if instruction.kind == InstructionKind.Elif:
                if prevInstruction is None or prevInstruction.kind not in [InstructionKind.If, InstructionKind.Elif]:
                    raise InvalidElifError(instruction.anchor)

            exprType = self._verify_expression_ast(container, localVars, instruction.expression.ast)

            # Make sure the return type evaluates to bool.
            nativeBoolType = self.native_bool_typename(container.references)
            nativeBoolNR = self._ast_navigate_dependency(nativeBoolType, False)
            if exprType != nativeBoolNR:
                raise PredicateExpectedError(instruction.anchor)

            childScopeState = ScopeState.Loop if instruction.kind == InstructionKind.While else scopeState
            self._verify_instructions(container, localVars, instruction.instructions, childScopeState, returnTypeNR)
        elif instruction.kind == InstructionKind.Else:
            # We need an if or elif above.
            if prevInstruction is None or prevInstruction.kind not in [InstructionKind.If, InstructionKind.Elif]:
                raise InvalidElseError(instruction.anchor)

            self._verify_instructions(container, localVars, instruction.instructions, scopeState, returnTypeNR)
        elif instruction.kind == InstructionKind.For:
            for forInit in instruction.forInits:
                self._verify_expression_ast(container, localVars, forInit.ast)

            for forPred in instruction.forPredicates:
                self._verify_expression_ast(container, localVars, forPred.ast)

            for forStep in instruction.forSteps:
                self._verify_expression_ast(container, localVars, forStep.ast)

            self._verify_instructions(container, localVars, instruction.instructions, ScopeState.Loop, returnTypeNR)

    def _verify_instructions(self, container, localVars, instructions, scopeState, returnTypeNR):
        """
        Verify all child instructions of an instruction.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            localVars (dict): The local variables.
            instructions ([objects.Instruction]): The instructions to verify.
            scopeState (linker.ScopeState): The current scope state.
            prevInstruction (objects.Instruction): The previous instruction in the same scope.
            returnTypeNR (linker.AstNavigationResult): The return type navigation result.
        """
        prevInstruction = None
        for instruction in instructions:
            self._verify_instruction(container, localVars, instruction, scopeState, prevInstruction, returnTypeNR)
            prevInstruction = instruction

    def _verify_expression_ast(self, container, localVars, ast):
        """
        Verify an expression.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            localVars (dict): Visible variable declarations.
            ast (objects.ExpressionAST): The expression AST to verify.
        Returns:
            linker.NavigationResult or None: The navigation result after searching for the type of the expression.
        """
        newLocalVars = {}

        result = self._verify_expression_ast_recursive(container, localVars, ast, newLocalVars)
        localVars.update(newLocalVars)

        return result

    def _verify_expression_ast_recursive(self, container, localVars, ast, newLocalVars, *, isOptional=False, lhs=None, isAssignment=False):
        """
        Verify an expression.

        Args:
            container (objects.Locatable): The locatable container (parent) object.
            localVars (dict): Visible variable declarations.
            ast (objects.ExpressionAST): The expression AST to verify.
            newLocalVars (dict): New local variables.
            isOptional (bool): Indicates whether the result of this function can be optionally None.
            lhs (linker.AstNavigationResult or None): The LHS in the AST. This can be a struct or namespace for example.
            isAssignment (bool): True, if this is an assignment. Otherwise False.
        Returns:
            linker.NavigationResult or None: The navigation result after searching for the type of the expression.
        """
        atom = ast.atom
        children = ast.children

        # Validate the AST.
        assert(atom.kind in self._astVerifiers)

        # Invoke the verification handler.
        return self._astVerifiers[atom.kind](container, atom, children, localVars, newLocalVars, isOptional, lhs, isAssignment)

    def navigate_alias_target(self, navResult):
        """
        Navigate to the next target type of an alias.

        This can be used after navigating to a location to find the next target type.

        Args:
            navResult (linker.NavigationResult): The previous navigation result to continue the search from.
        Returns:
            linker.NavigationResult or None: The next navigation result or None if there was no resolvable alias.
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
        baseNavResult = self.navigate_dependency(targetTypenameDependency)

        # Promote base type to array type.
        dimNavResult = baseNavResult.dependency.as_array(dependency.locatable.targetTypename.dims)
        return NavigationResult(baseNavResult.resolvedDependencyLocation, dimNavResult)

    def navigate_alias_base(self, navResult):
        """
        Navigate to the base type of an alias.

        This can be used after navigating to a location to find the last target type.

        Args:
            navResult (linker.NavigationResult): The previous navigation result to continue the search from.
        Returns:
            linker.NavigationResult: The next navigation result.
        """
        result = navResult
        visitedResults = set()
        dependencyChain = [result.dependency.location]
        while True:
            if result.dependency.location in visitedResults:
                raise CircularDependencyError(dependencyChain)

            visitedResults.add(result.dependency.location)
            nextResult = self.navigate_alias_target(result)
            if nextResult is None:
                return result

            dependencyChain.append(nextResult.dependency.location)
            result = nextResult

    def try_navigate_dependency(self, dependency):
        """
        Navigate using a dependency as the target.

        Args:
            dependency (linker.Dependency): The dependency to lookup.
        Returns:
            linker.NavigationResult or None: The navigation result.
        """
        navResult = self.links.get(dependency, None)
        if navResult is not None:
            return navResult

        navResult = self.try_navigate(dependency.locatable.anchor, dependency.locatable.references, dependency.locatable.parent, dependency.baseLocation)
        if navResult is not None:
            self.links[navResult.dependency] = navResult
            return navResult

        return None

    def navigate_dependency(self, dependency):
        """
        Navigate using a dependency as the target.

        Args:
            dependency (linker.Dependency): The dependency to lookup.
        Returns:
            linker.NavigationResult or None: The navigation result or None, if not match was found.
        """
        navResult = self.try_navigate_dependency(dependency)
        if navResult is None:
            raise DependencyNotFoundError(dependency.locatable.anchor, dependency.location)
        return navResult

    def _solve_location_conflicts(self):
        """Solve all known location conflicts."""
        for conflict in self.locationConflicts:
            d0 = conflict.firstDependency
            d1 = conflict.secondDependency
            d0Params = d0.resolve_parameter_locations(self)
            d1Params = d1.resolve_parameter_locations(self)
            isFalsePositive = any(p0 != p1 for p0, p1 in zip(d0Params, d1Params))
            if not isFalsePositive:
                d0Params = d0.resolve_parameter_locations(self)
                d1Params = d1.resolve_parameter_locations(self)
                raise DuplicateParameterSignatureError(d0.locatable.anchor, d1.locatable.anchor)

        self.locationConflicts = []

class LinkedProject:
    """
    A fully linked project.

    Attributes:
        linkableProject (linker.LinkableProject): The project to link.
        sortedTypeDependencies ([linker.Dependency]): All type dependencies (dependency-sorted).
        functions ([linker.Dependency]): All function dependencies (unsorted).
        orderedLibraryNames ([str]): The ordered library name list.
    """

    def __init__(self, linkableProject, orderedLibraryNames):
        """
        Initialize the object.

        Args:
            linkableProject (linker.LinkableProject): The project to link.
            orderedLibraryNames ([str]): The ordered library name list.
        """
        self.orderedLibraryNames = orderedLibraryNames
        self.linkableProject = linkableProject
        self.sortedTypeDependencies = []
        self._sort_type_dependencies()
        self.functions = []

    def _sort_type_dependencies(self):
        """
        Return the sorted type dependencies from the unlinked project.

        Returns:
            list: A sorted list of type dependencies.
        """
        typeDag = nx.DiGraph()
        for resolvedLocation in self.linkableProject.resolvedObjects.values():
            self._create_type_dag(typeDag, resolvedLocation)

        # All dependencies have to resolve nicely
        try:
            sortedTypeDependencies = list(nx.topological_sort(typeDag))
        except nx.exception.NetworkXUnfeasible:
            cycle = nx.find_cycle(typeDag)

            # Circular dependency string
            dependencyChain = []
            for p in reversed(cycle):
                dependencyChain.append(p[0].location)
            dependencyChain.append(p[1].location)

            raise CircularDependencyError(dependencyChain)

        self.sortedTypeDependencies = list(sorted(sortedTypeDependencies, key=lambda dependency: self.orderedLibraryNames.index(dependency.libName)))

    def _create_type_dag(self, typeDag, resolvedLocation):
        """
        Insert a resolved location with all its dependencies and sublocations into the given graph.

        Args:
            typeDag: The type reference DAG (Type -> Type).
            resolvedLocation (linker.ResolvedLocation): The resolved location.
        """
        for dependency in resolvedLocation.dependencies:
            locatable = dependency.locatable
            if isinstance(locatable, Struct):
                typeDag.add_node(dependency)

                for locatable in locatable.locatables:
                    if isinstance(locatable, MemberList):
                        navResult = self.linkableProject.links[Dependency(locatable.typename)]
                        typeDag.add_edge(navResult.dependency, dependency) # MemberType -> Struct
            elif isinstance(locatable, Alias):
                typeDag.add_node(dependency)
                aliasDependency = dependency
                while isinstance(locatable, Alias):
                    aliasDependency = dependency
                    navResult = self.linkableProject.links[Dependency(locatable.targetTypename)]
                    typeDag.add_edge(navResult.dependency, aliasDependency) # TargetType -> Alias
                    dependency = navResult.dependency
                    locatable = dependency.locatable

                if isinstance(locatable, Struct):
                    typeDag.add_edge(dependency, aliasDependency) # Struct -> Alias

        # And repeat.
        for subLocationName, subLocation in resolvedLocation.subLocations.items():
            self._create_type_dag(typeDag, subLocation)