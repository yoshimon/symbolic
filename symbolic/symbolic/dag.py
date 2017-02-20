"""Contains classes to resolve dependencies in symbolic."""

# Built-in
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

class Dependency:
    """
    A dependency within a project.

    Attributes:
        obj (object): The object behind the dependency.
        location (objects.Location): The location of the object in the library.
        references ([objects.Reference]): The references that are seen by this dependency.
        isPrivate (bool): True, if the object is private. Otherwise False.
        isDeprecated (bool): True, if the object is deprecated. Otherwise False.
    """

    def __init__(self, references, obj):
        """
        Initialize the object.

        Args:
            references ([objects.Reference]): The references list.
            obj (object): The object behind the dependency.
        """
        self.obj = obj
        self.location = obj.location()
        self.references = references
        
        # Two system annotations are valid for all dependencies: private, deprecate
        self.isPrivate = Annotation.has('private', obj.sysAnnotations) if obj is isinstance(obj, Named) else False
        self.isDeprecated = Annotation.has('deprecated', obj.sysAnnotations) if obj is isinstance(obj, Named) else False

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

class NavigationResult:
    """
    A result of a navigation operation.

    Attributes:
        libName (str): The library name.
        resolvedDependencyLocation ([dag.ResolvedDependencyLocation]): The resolved dependency location.
    """

    def __init__(self, libName, resolvedDependencyLocation):
        """
        Initialize the object.

        Args:
            libName (str): The library name.
            resolvedDependencyLocation ([dag.ResolvedDependencyLocation]): The resolved dependency location.
        """
        self.libName = libName
        self.resolvedDependencyLocation = resolvedDependencyLocation

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

    def __init__(self):
        """Initialize the object."""
        self.unresolvedDependencies = list()
        self.libraries = {} # The libraries lookup table
        self.resolvedObjects = defaultdict(ResolvedDependencyLocation) # Maps each library to a set of resolved objects
        self.links = {} # Maps unresolved dependencies to their resolved counterparts.
        self.locationConflicts = [] # Locations that point to the same endpoint (conflicts).
        self.libName = None # The name of the library that is being monitored.
        self.templateLinks = {} # Maps locations to template instantiation results.
        self.functions = [] # An internal cache of function objects inside the current library.
        self._libLocationNavResults = {} # Maps tags (object Ids) to their resolved navigation result.

        # Initialize function table that maps ExpressionAtomKinds to AST verification functions.
        self._astVerifiers = {
            ExpressionAtomKind.Var: self._verify_ast_var,
            ExpressionAtomKind.Number: self._verify_ast_number,
            ExpressionAtomKind.FunctionEnd: self._verify_ast_function,
            ExpressionAtomKind.ArrayEnd: self._verify_ast_array,
            ExpressionAtomKind.TemplateEnd: self._verify_ast_template,
            ExpressionAtomKind.UnaryOp: self._verify_ast_unary_op,
            ExpressionAtomKind.BinaryOp: self._verify_ast_binary_op
        }

    def _verify_ast_var(self, atom, children, localVars, newLocalVars, isOptional):
        """
        Verify an AST with a variable atom type.

        Args:
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
            localVars (dict): Lookup table that maps variable names to resolved locations.
            newLocalVars
        Returns:
            dag.NavigationResult: The location of the resulting type of this AST.
        """
        # Lookup the resolved variable type.
        varNR = localVars.get(str(atom.token), None)
        if not isOptional and varNR is None:
            raise VariableNotFoundError(atom.token)

        # Return the navigation result.
        return varNR

    def _verify_ast_number(self, atom, children, localVars, newLocalVars, isOptional):
        """
        Verifies an AST with a number atom type.

        Args:
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
        Returns:
            dag.NavigationResult: The location of the resulting type of this AST.
        """
        pass

    def _verify_ast_function(self, atom, children, localVars, newLocalVars, isOptional):
        """
        Verify an AST with a function atom type.

        Args:
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
        Returns:
            dag.NavigationResult: The location of the resulting type of this AST.
        """
        pass

    def _verify_ast_array(self, atom, children, localVars, newLocalVars, isOptional):
        """
        Verifies an AST with an array atom type.

        Args:
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
        Returns:
            dag.NavigationResult: The location of the resulting type of this AST.
        """
        pass

    def _verify_ast_template(self, atom, children, localVars, newLocalVars, isOptional):
        """
        Verify an AST with a template atom type.

        Args:
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
        Returns:
            dag.NavigationResult: The location of the resulting type of this AST.
        """
        pass

    def _verify_ast_unary_op(self, atom, children, localVars, newLocalVars, isOptional):
        """
        Verifies an AST with a unary operator atom type.

        Args:
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
        Returns:
            dag.NavigationResult: The location of the resulting type of this AST.
        """
        pass

    def _verify_ast_binary_op(self, atom, children, localVars, newLocalVars, isOptional):
        """
        Verify an AST with a binary operator atom type.

        Args:
            atom (objects.ExpressionAtom): The root atom.
            children (objects.ExpressionAST): The child nodes.
        Returns:
            dag.NavigationResult: The location of the resulting type of this AST.
        """
        assert(len(children) == 2)

        left, right = children[0], children[1]

        if atom.token.isLValueOp and not self._is_lvalue(left):
            raise LValueRequiredError(atom.token.anchor)
            
        # If this is the = operator then the LHS does not have to have a deducable type.
        # A missing LHS type would indicate that we have a new variable declaration in that case.
        isAssignmentOp = atom.token == "="
            
        leftNR = self._verify_expression_ast(localVars, newLocalVars, left, isOptional=isAssignmentOp)
        rightNR = self._verify_expression_ast(localVars, newLocalVars, right)

        if leftNR is None:
            assert(isAssignmentOp)
                
            if left.atom.kind == ExpressionAtomKind.Var:
                # New variable.
                varToken = left.atom.token
                varName = str(varToken)
                if varName in newLocalVars:
                    raise VariableAlreadyExistsError(varToken)

                newLocalVars[varName] = rightNR
            else:
                # Must be an extension.
                pass

        # Lookup the operator now.
    
    def navigate(self, errorAnchor, requester, references, parent, location, tag):
        """
        Navigate to a location.

        Args:
            errorAnchor (lexer.Anchor): The anchor to use in case an exception is thrown.
            requester (dag.Dependency): The dependency that requests the navigation.
            references ([objects.Reference]): The library references.
            parent (objects.Locatable or None): The parent object to start the search at.
                The navigation will start the the parent and, on failure, move up the
                hierarchy until a match is found.
            location (objects.Location): The location.
            tag (int): A tag to associate with this navigation operation. Used for caching
                navigation results.
        Returns:
            dag.NavigationResult: The resolved location.
        """
        # Navigate the library tree first (explicit library name)
        # Fall back to reference order in unit, if no match possible
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

        locationsWithoutLibName.append(Location(locationWithoutLibName))

        # Find the library and the object in the library
        lookup = None
        resolvedLibName = None
        locationFound = False
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
                    possibleMatches = filter(lambda dependency: i < len(dependency.location) and rl.might_be_equal_to(dependency.location[i]), dependencies)
                    if not possibleMatches:
                        continue

                    # Find the best match out of the possible matches by counting the number of
                    # partial matches. The highest match count wins.
                    countPartialMatches = lambda templateParameters: functools.reduce(lambda numMatches, p: numMatches + 1 if p.partialMatch is not None else 0, templateParameters, 0) 
                    dependencyCmp = lambda a, b: countPartialMatches(b.location[i].templateParameters) - countPartialMatches(a.location[i].templateParameters)
                    bestMatches = sorted(possibleMatches, key=functools.cmp_to_key(dependencyCmp))

                    for dependency in bestMatches:
                        dependencyRL = dependency.location[i]
                        dependencyObj = dependency.obj

                        # If it is an alias then we have to lookup the aliased type instead
                        # since aliases don't have any sublocations.
                        if isinstance(dependencyObj, Alias):
                            # But only do that if this is not the last relative location
                            # since we are actually looking for the Alias in that case.
                            if i != len(location.path) - 1:
                                aliasNavResult = NavigationResult(resolvedLibName, resolvedDependencyLocation)
                                aliasNavResult = self.navigate_alias_base(aliasNavResult)
                                libName = aliasNavResult.libName
                                resolvedDependencyLocation = aliasNavResult.resolvedDependencyLocation
                        elif isinstance(dependencyObj, Template):
                            # Lazily instantiate templates, if we encounter one.
                            # Make sure we did not instantiate the template already.
                            templateInstanceArgs =  ", ".join(str(p) for p in rl.templateParameters)
                            requesterReferences = requester.references
                            importLibs = ", ".join(str(ref) for ref in requesterReferences)
                            importLibs = " using {0}".format(importLibs) if importLibs else importLibs
                            dependencyLocationStr = "{0} with <{1}>{2}".format(str(dependency.location), templateInstanceArgs, importLibs)

                            if dependencyLocationStr not in self.templateLinks:
                                # Generate the translation unit for the template.
                                templateSrc = dependencyObj.generate_translation_unit()

                                # Run the pre-processor on the source.
                                # TODO: run pre-processor based on the location
                                ppTemplateSrc = templateSrc # self.preprocessor.run(templateSrc, LIBNAME, PPT)

                                # Lex the unit so we can parse it.
                                lexer = SymbolicLexer(libName=self.libName, fileName="$Templates/{0}".format(str(dependencyObj.token)))

                                # Plugin the template substitutions for the lexer.
                                templateSubs = { str(dependencyRL.templateParameters[i].token): str(parameter.token)[1:-1] for i, parameter in enumerate(rl.templateParameters) }

                                # Generate a parsable token stream now.
                                srcFileTokens = lexer.tokenize(ppTemplateSrc, subs=templateSubs)
                                srcFileTokens = lexer.concatenate_tokens(srcFileTokens)

                                # Analyze the token stream.
                                parser = UnitParser(lexer.libName, lexer.fileName, srcFileTokens)
                                parseResult = parser.parse()

                                # Lookup the template object
                                templateObj = parseResult.rootNamespace.objects[0]
                                templateObj.parent = dependencyObj.parent
                                templateObj.grandParent = dependencyObj.grandParent
                                templateObj.grandParentWithoutRoot = dependencyObj.grandParentWithoutRoot

                                # Bind the location to a template.
                                self.templateLinks[dependencyLocationStr] = templateObj

                                # Insert it into the collection so we can look it up.
                                self.insert_unit(requesterReferences, parseResult.rootNamespace)
                        
                            # Use template links to jump to the right location, which is anonymous.
                            templateObj = self.templateLinks[dependencyLocationStr]
                            templateNavResult = self.navigate(dependencyObj.token.anchor, requester, references, templateObj.parent, templateObj.location(), id(templateObj))
                            templateAliasNavResult = self.navigate_alias_base(templateNavResult)
                            resolvedDependencyLocation = templateAliasNavResult.resolvedDependencyLocation

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
            raise DependencyNotFoundError(errorAnchor, location)

        result = NavigationResult(resolvedLibName, lookup)

        # Cache the result.
        # This is used to lookup function parameter typename locations later.
        self._libLocationNavResults[tag] = result

        return result

    def insert(self, references, obj):
        """
        Insert an object into the dependency collection.

        Args:
            references ([objects.Reference]): The references list.
            obj (object): The object to insert.
        """
        # Create and cache the dependency
        dependency = Dependency(references, obj)
        dependencyLocation = dependency.location
        rl = dependencyLocation[-1]

        if rl.kind == LocationKind.Unresolved:
            self.unresolvedDependencies.append(dependency)
        else:
            # Navigate to parent
            navResult = self.navigate(obj.token.anchor, dependency, references, obj.grandParentWithoutRoot, dependencyLocation[:-1], id(obj))
            lookup = navResult.resolvedDependencyLocation.subLocations

            # Make sure that no duplicate object in this namespace exists
            if rl.name in lookup:
                existingDependencies = lookup[rl.name].dependencies

                # See if parameter signature and template parameter count matches
                for otherDependency in existingDependencies:
                    otherDependencyRL = otherDependency.location[-1]

                    # If the location kinds are different then it is ambigous
                    if rl.kind != otherDependencyRL.kind:
                        raise DuplicateNameError(obj.anchor, otherDependency.obj.anchor)

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

    def insert_unit(self, references, rootNamespace):
        """
        Insert an unresolved translation unit into the collection.

        Args:
            references ([objects.Reference]): The library import references.
            rootNamespace (objects.Namespace): The global namespace.
        """
        # Create a dependency for every object
        objs = deque(rootNamespace.objects)
        while objs:
            obj = objs.popleft()

            # Register it
            self.insert(references, obj)

            # Recursive objects (Namespaces, Functions)
            children = getattr(obj, "objects", None)
            objs += children if children is not None else []

            # Connect immediate unresolved dependencies
            if isinstance(obj, Function):
                # The unknown return type
                self.insert(references, obj.returnTypename)

                # The unknown parameter types
                for parameter in obj.parameters:
                    self.insert(references, parameter.typename)

                # Remember this function for instruction verification later.
                self.functions.append(obj)
            elif isinstance(obj, Alias):
                # The unknown typename
                self.insert(references, obj.targetTypename)

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
                obj = dependency.obj

                # The name has to be resolvable by now
                # Catching unresolved objects will spawn templates, if encountered
                self.links[dependency] = self.navigate(obj.anchor, dependency, dependency.references, obj.parent, dependency.location, id(obj))

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
            pId = id(p.typename)
            pNR = self._libLocationNavResults[pId]
            localVars[str(p.token)] = pNR

        for obj in func.objects:
            if isinstance(obj, Instruction):
                self._verify_instruction(localVars, obj)

    def _verify_instruction(self, localVars, instruction):
        """
        Verify an instruction.

        Args:
            localVars (dict): The local variables.
            instruction (objects.Instruction): The instruction to verify.
        """
        newLocalVars = {}

        # Verify the AST.
        if instruction.kind == InstructionKind.Expression:
            # We don't care about the expression type.
            self._verify_expression_ast(localVars, newLocalVars, instruction.expression.ast)
        elif instruction.kind in [InstructionKind.Break, InstructionKind.Continue]:
            # Make sure we are inside a loop.
            if not state != ScopeState.Loop:
                raise NotInsideLoopError(instruction.token.anchor)
        elif instruction.kind == InstructionKind.Return:
            exprTypeLocation = self._verify_expression_ast(localVars, newLocalVars, instruction.expression.ast)
            
            # Return statements are not allowed to introduce new variables.
            if localVars:
                raise DeclarationInReturnError(instruction.token.anchor)

            # Make sure the expression type matches the function return type.
            func = instruction.parent
            assert(isinstance(func, Function))
            return exprTypeLocation == func.returnTypename
        elif instruction.kind == InstructionKind.If:
            exprType = self._verify_expression_ast(localVars, newLocalVars, instruction.expression.ast)

            # Make sure the return type evaluates to bool.
            
        elif instruction.kind == InstructionKind.Elif:
            pass

    def _verify_expression_ast(self, localVars, newLocalVars, ast, *, isOptional=False):
        """
        Verify an expression.

        Args:
            localVars (dict): Visible variable declarations.
            newLocalVars (dict): New variable declarations.
            ast (objects.ExpressionAST): The expression AST to verify.
            isOptional (bool): Indicates whether the result of this function can be optionally None.
        Returns:
            dag.NavigationResult or None: The navigation result after searching for the type of the expression.
        """
        atom = ast.atom
        children = ast.children

        # Validate the AST.
        assert(atom.kind in self._astVerifiers)

        # Invoke the verification handler.
        return self._astVerifiers[atom.kind](atom, children, localVars, newLocalVars, isOptional)

    @staticmethod
    def _is_lvalue(ast):
        """
        Returns whether an AST contains an l-value only.
        
        Args:
            ast (objects.ExpressionAST): The AST to inspect.
        Returns:
            bool: True, if the AST contains an l-value. Otherwise False.
        """
        # Look at LHS of . or the atom itself if it is not a member.
        varAtomKind = ast.children[1].atom.kind if str(ast.atom.token) in "." else ast.atom.kind
        return varAtomKind == ExpressionAtomKind.Var

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
        dependency = next((dependency for dependency in dependencies if isinstance(dependency.obj, Alias)), None)
        if dependency is None:
            return None

        # Navigate to the target type.
        targetTypename = dependency.obj.targetTypename
        return self.navigate(targetTypename.anchor, dependency, dependency.references, dependency.obj.parent, targetTypename.location(), id(targetTypename))

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

    def navigate_dependency(self, dependency):
        """
        Navigate using a dependency as the target.

        Args:
            dependency (dag.Dependency): The dependency to lookup.
        Returns:
            dag.NavigationResult: The navigation result.
        """
        return self.links[dependency] if dependency in self.links else self.navigate(dependency.obj.token, dependency, dependency.references, dependency.obj.parent, dependency.location, id(dependency.obj))

    def _solve_location_conflicts(self):
        """Solve all known location conflicts."""
        # Try to resolve location conflicts now.
        for conflict in self.locationConflicts:
            # Look at both conflicts:
            # They have to be parameter conflicts, i.e. f(int) and f(int).
            dep0 = conflict.firstDependency
            dep1 = conflict.secondDependency

            dep0Params = dep0.location[-1].parameters
            dep1Params = dep1.location[-1].parameters

            # Assume this is a conflict
            isConflict = True

            # Look at all parameter types in the conflict signatures and
            # lookup the resolved locations for the types.
            for p0, p1 in zip(dep0Params, dep1Params):
                # Create a dependency for the parameters
                p0Dep = Dependency(dep0.references, p0)
                p1Dep = Dependency(dep1.references, p1)

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
                raise DuplicateParameterSignatureError(dep0.obj.token.anchor, dep1.obj.token.anchor)

        self.locationConflicts = []

class ProjectDependencyGraph:
    pass