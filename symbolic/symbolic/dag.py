﻿"""Contains classes to resolve dependencies in symbolic."""

# Built-in
from itertools import chain
from collections import deque, defaultdict

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
        references (list of objects.Reference): The references that are seen by this dependency.
        isPrivate (bool): True, if the object is private. Otherwise False.
        isDeprecated (bool): True, if the object is deprecated. Otherwise False.
    """

    def __init__(self, references, obj):
        """
        Initialize the object.

        Args:
            references (list of objects.Reference): The references list.
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
        firstDependency (objects.Dependency): The first dependency.
        secondDependency (objects.Dependency): The second dependency.
    """

    def __init__(self, firstDependency, secondDependency):
        """
        Initialize the object.
        
        Args:
            firstDependency (objects.Dependency): The first dependency.
            secondDependency (objects.Dependency): The second dependency.
        """
        self.firstDependency = firstDependency
        self.secondDependency = secondDependency

class ResolvedDependencyLocation:
    """
    A collection of dependencies that have been resolved to a location.

    Attributes:
        dependencies (list of objects.Dependency): The dependencies at this location.
        subLocations (dict of (str, objects.ResolvedDependencyLocation)): The sub-locations.    
    """

    def __init__(self, dependencies=None, subLocations=None):
        """
        Initialize the object.

        Args:
            dependencies (list of objects.Dependency): The dependencies at this location.
            subLocations (dict of (str, objects.ResolvedDependencyLocation)): The sub-locations.
        """
        self.dependencies = dependencies if dependencies else []
        self.subLocations = subLocations if subLocations else {}

class NavigationResult:
    """
    A result of a navigation operation.

    Attributes:
        libName (str): The library name.
        resolvedDependencyLocation (list of objects.ResolvedDependencyLocation): The resolved dependency location.
    """

    def __init__(self, libName, resolvedDependencyLocation):
        """
        Initialize the object.

        Args:
            libName (str): The library name.
            resolvedDependencyLocation (list of objects.ResolvedDependencyLocation): The resolved dependency location.
        """
        self.libName = libName
        self.resolvedDependencyLocation = resolvedDependencyLocation

class ProjectDependencyCollection:
    """
    A colllection of dependencies within a project.

    Attributes:
        unresolvedDependencies (set of objects.Dependency): A set of unresolved dependencies.
        libraries (dict): The libraries lookup table.
        resolvedObjects (defaultdict): Maps each library to a list of resolved objects
        links (dict): Maps unresolved dependencies to their resolved counterparts
        locationConflicts (list of objects.LocationConflict): A list of location conflicts.
    """

    def __init__(self):
        """Initialize the object."""
        self.unresolvedDependencies = list()
        self.libraries = {} # The libraries lookup table
        self.resolvedObjects = defaultdict(ResolvedDependencyLocation) # Maps each library to a set of resolved objects
        self.links = {} # Maps unresolved dependencies to their resolved counterparts
        self.locationConflicts = [] # Locations that point to the same endpoint (conflicts)
        self.libName = None # The name of the library that is being monitored

    def navigate(self, errorAnchor, references, location):
        """
        Navigate to a location.

        Args:
            errorAnchor (objects.Anchor): The anchor to use in case an exception is thrown.
            references (list of objects.Reference): The library references.
            location (objects.Location): The location.
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

        # Find the library and the object in the library
        lookup = None
        resolvedLibName = None
        locationFound = False
        for libName in libNameGen:
            # Assume this library contains the dependency
            resolvedLibName = libName
            lookup = self.resolvedObjects[libName]

            # Loop through all sublocations and verify the assumption above
            allSubLocationsFound = True
            for i, rl in enumerate(location[offset:]):
                if rl.name not in lookup.subLocations:
                    allSubLocationsFound = False
                    break

                # The dependencies associated with this name
                resolvedDependencyLocation = lookup.subLocations[rl.name]
                dependencies = resolvedDependencyLocation.dependencies

                # ADL (parameter signature)
                dependency = next((d for d in dependencies if rl.parameters == d.location[i].parameters), None)

                if not dependency:
                    raise OverloadNotFoundError(errorAnchor)

                dependencyRL = dependency.location[i]

                # Make sure the template arguments match
                if len(rl.templateParameters) != len(dependencyRL.templateParameters):
                    raise TemplateMismatchError(errorAnchor)

                # Instantiate the template
                dependencyObj = dependency.obj
                if isinstance(dependencyObj, Template):
                    # Generate the translation unit for the template
                    templateSrc = dependencyObj.generate_translation_unit()

                    # Run the pre-processor on the source
                    ppTemplateSrc = self.preprocessor.run(templateSrc)

                    # Lex the unit
                    lexer = SymbolicLexer(libName=self.libName, fileName=str(dependencyObj.token))

                    # Modify the template substitutions of the lexer
                    lexer.subs = { dependencyRL.templateParameters[i].token.text: parameter.text[1:-1] for i, parameter in enumerate(rl.templateParameters) }

                    # Lex the generated unit
                    srcFileTokens = lexer.tokenize(ppTemplateSrc)

                    # Parse the unit
                    parser = UnitParser(lexer.libName, lexer.fileName, srcFileTokens)
                    templateReferences, templateRootNamespace = parser.parse()
                    
                    # Insert it into the collection
                    self.insert_unit(templateReferences, templateRootNamespace)

                # Step down this namespace
                lookup = resolvedDependencyLocation

            # If we verified the location we can stop the search
            locationFound = allSubLocationsFound
            if locationFound:
                break

        if not locationFound:
            raise DependencyNotFoundError(errorAnchor, location)

        return NavigationResult(resolvedLibName, lookup)

    def insert(self, references, obj):
        """
        Insert an object into the dependency collection.

        Args:
            references (list of objects.Reference): The references list.
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
            navResult = self.navigate(obj.token.anchor, references, dependencyLocation[:-1])
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
                    if len(rl.templateParameters) == len(otherDependencyRL.templateParameters):
                        if len(rl.parameters) == len(otherDependencyRL.parameters):
                            # It might be a conflict if the parameter modifiers match
                            # The parameter typenames need to be resolved later to verify the conflict
                            isConflict = Algorithm.all_sequence(rl.parameters, otherDependencyRL.parameters,
                                                                lambda p0, p1: p0.isRef == p1.isRef)

                            if isConflict:
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
            references (list of objects.Reference): The library import references.
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
            elif isinstance(obj, Alias):
                # The unknown typename
                self.insert(references, obj.targetTypename)

    def begin_library(self, libName):
        """
        Begin collecting dependencies for a new library.
        
        Args:
            libName (str): The name of the library.
        """
        print("Processing library {0}...".format(libName))

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

        # Remember the library name
        self.libName = libName

    def end_library(self):
        """End collecting dependencies for the current library."""
        self._resolve()

        # Close this library
        self.libName = None

    def _resolve(self):
        """Resolve all dependencies."""
        print("Resolving library dependencies...")

        if self.unresolvedDependencies:
            # Traverse from top to bottom
            unresolvedDependencies = deque(self.unresolvedDependencies)
            
            # Clear the unresolved dependencies, they are in the queue
            self.unresolvedDependencies.clear()
            
            while unresolvedDependencies:
                dependency = unresolvedDependencies.popleft()

                # The name has to be resolvable by now
                # Catching unresolved objects will spawn templates, if encountered
                self.links[dependency] = self.navigate(dependency.obj.anchor, dependency.references, dependency.location)

                # Add the unresolved dependencies to the queue
                unresolvedDependencies += self.unresolvedDependencies

                # And clear the cache again
                self.unresolvedDependencies.clear()

            # Now that each dependency has been resolved
            # we solve location conflicts due to clashing parameter
            # signatures by comparing the types
            self._solve_location_conflicts()

    def navigate_alias_target(self, navResult):
        """
        Navigate to the next target type of an alias.
        
        This can be used after navigating to a location to find the next target type.

        Args:
            navResult (dag.NavigationResult): The previous navigation result to continue the search from.
        Returns:
            dag.NavigationResult: The next navigation result.
        """
        if navResult is None:
            return None

        # There should only be one alias dependency at this location
        dependencies = navResult.resolvedDependencyLocation.dependencies
        if len(dependencies) != 1:
            return None

        # It has to be an alias
        dependency = dependencies[0]
        obj = dependency.obj
        if not isinstance(obj, Alias):
            return None

        # Navigate to the target type
        targetTypename = obj.targetTypename
        return self.navigate(targetTypename.anchor, dependency.references, targetTypename.location())

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

    def _solve_location_conflicts(self):
        """Solve all known location conflicts."""
        # Try to resolve location conflicts now
        for conflict in self.locationConflicts:
            # Look at both conflicts
            # They have to be parameter conflicts, i.e. f(int) and f(int)
            dep0 = conflict.firstDependency
            dep1 = conflict.secondDependency

            dep0Params = dep0.location[-1].parameters
            dep1Params = dep1.location[-1].parameters

            # Assume this is a conflict
            isConflict = True

            # Look at all parameter types in the conflict signatures
            # and lookup the resolved locations for the types.
            for p0, p1 in zip(dep0Params, dep1Params):
                # Create a dependency for the parameters
                p0Dep = Dependency(dep0.references, p0)
                p1Dep = Dependency(dep1.references, p1)

                # Lookup link of parameter dependency
                p0Resolved = self.links[p0Dep] if p0Dep in self.links else self.navigate(p0Dep.obj.token, p0Dep.references, p0Dep.location)
                p1Resolved = self.links[p1Dep] if p1Dep in self.links else self.navigate(p1Dep.obj.token, p1Dep.references, p1Dep.location)

                # Navigate aliases down to the base type so that we can compare the locations for a conflict below.
                # Aliases are unparamterized and hence unique.
                p0Resolved = self.navigate_alias_base(p0Resolved)
                p0ResolvedLocation = p0Resolved.resolvedDependencyLocation
                
                p1Resolved = self.navigate_alias_base(p1Resolved)
                p1ResolvedLocation = p1Resolved.resolvedDependencyLocation

                # If the resolved locations differ this is not a conflict
                if p0ResolvedLocation != p1ResolvedLocation:
                    isConflict = False
                    break

            if isConflict:
                raise DuplicateParameterSignatureError(dep0.obj.token.anchor, dep1.obj.token.anchor)

    def to_graph(self):
        pass

class ProjectDependencyGraph:
    pass