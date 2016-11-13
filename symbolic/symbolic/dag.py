# Built-in
from itertools import chain
from collections import deque

# Library
import networkx as nx
import matplotlib.pyplot as plt

# Project
from symbolic.exceptions import *
from symbolic.language import *
from symbolic.objects import *
from symbolic.parsers import *

class Dependency:
    '''A dependency within a project.'''
    def __init__(self, references, obj):
        '''
        Initialize the object.
        
        Args:
            references (list(Reference)): The references list.
            obj (object): The object behind the dependency.
        '''
        self.obj = obj
        self.location = obj.location()
        self.references = references
        
        # Two system annotations are valid for all dependencies: private, deprecate
        self.isPrivate = Annotation.has('private', obj.sysAnnotations) if obj is isinstance(obj, Named) else True
        self.isDeprecated = Annotation.has('deprecated', obj.sysAnnotations) if obj is isinstance(obj, Named) else False

class LocationConflict:
    '''A conflict between two locations.'''
    def __init__(self, firstDependency, secondDependency):
        '''
        Initialize the object.
        
        Args:
            firstDependency (Dependency): The first dependency.
            secondDependency (Dependency): The second dependency.
        '''
        self.firstDependency = firstDependency
        self.secondDependency = secondDependency

class ResolvedDependencyLocation:
    '''A collection of dependencies that have been resolved to a location.'''
    def __init__(self, dependencies, subLocations):
        '''
        Initialize the object.

        Args:
            dependencies (list(Dependency)): The dependencies at this location.
            subLocations (dict(str, ResolvedDependencyLocation)): The sub-locations.
        '''
        self.dependencies = dependencies
        self.subLocations = subLocations

class NavigationResult:
    '''A result of a navigation operation.'''
    def __init__(self, libName, resolvedDependencyLocation):
        '''
        Initialize the object.

        Args:
            libName (str): The library name.
            resolvedDependencyLocation (list(ResolvedDependencyLocation)): The resolved dependency location.
        '''
        self.libName = libName
        self.resolvedDependencyLocation = resolvedDependencyLocation

class ProjectDependencyCollection:
    '''A colllection of dependencies within a project.'''
    def __init__(self, lexer):
        '''Initialize the object.'''
        self.lexer = lexer
        self.unresolvedDependencies = set()
        self.libraries = {} # The libraries lookup table
        self.resolvedObjects = {} # Maps each library to a list of resolved objects
        self.links = {} # Maps unresolved dependencies to their resolved counterparts
        self.locationConflicts = [] # Locations that point to the same endpoint (conflicts)

    def navigate(self, errorAnchor, references, location):
        '''
        Navigate to a location.

        Args:
            errorAnchor (Anchor): The anchor to use in case an exception is thrown.
            references (list(Reference)): The library references.
            location (Location): The location.
        Returns:
            dict: The resolved location.
        '''
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
        libNameGen = chain([self.lexer.libName], (str(ref) for ref in references)) if isImplicitRef else ['.'.join(rl.name for rl in location[:offset])]

        # Find the library and the object in the library
        lookup = None
        resolvedLibName = None
        locationFound = True
        for libName in libNameGen:
            lookup = self.resolvedObjects[libName]
            
            # Assume this library contains the dependency
            resolvedLibName = libName

            for i, rl in enumerate(location[offset:]):
                # The namespace has to exist
                if rl.name not in lookup:
                    locationFound = False
                    break

                # The dependencies associated with this name
                resolvedDependencyLocation = lookup[rl.name]
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

                    # Modify the template substitutions of the lexer
                    self.lexer.subs = { dependencyRL.templateParameters[i].token.text: parameter.text[1:-1] for i, parameter in enumerate(rl.templateParameters) }

                    # Lex the generated unit
                    srcFileTokens = self.lexer.tokenize(templateSrc)

                    # Parse the unit
                    parser = UnitParser(self.lexer, srcFileTokens)
                    templateReferences, templateRootNamespace = parser.parse()
                    
                    # Insert it into the collection
                    self.insert_unit(templateReferences, templateRootNamespace)

                # Step down this namespace
                lookup = resolvedDependencyLocation.subLocations

            if locationFound:
                break

        if not locationFound:
            raise DependencyNotFoundError(errorAnchor, location)

        return NavigationResult(resolvedLibName, lookup)

    def insert(self, references, obj):
        '''
        Insert an object into the dependency collection.

        Args:
            references (list(Reference)): The references list.
            obj (object): The object to insert.
        '''
        # Create and cache the dependency
        dependency = Dependency(references, obj)
        dependencyLocation = dependency.location
        rl = dependencyLocation[-1]

        if rl.kind == LocationKind.Unresolved:
            self.unresolvedDependencies.add(dependency)
        else:
            # Navigate to parent
            navResult = self.navigate(obj.token.anchor, references, dependencyLocation[:-1])
            lookup = navResult.resolvedDependencyLocation

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
                            # It might be a conflict if the modifiers match
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
        '''
        Insert an unresolved translation unit into the collection.

        Args:
            references (list(Reference)): The library import references.
            rootNamespace (Namespace): The global namespace.
        '''
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
                self.insert(references, obj.returnTypename)

                for parameter in obj.parameters:
                    self.insert(references, parameter)
            elif isinstance(obj, Alias):
                self.insert(references, obj.targetTypename)

    def begin_library(self):
        '''Begin collecting dependencies for a new library.'''
        # Create an empty entry in the global dict
        # Breakup the library name and insert it
        # Each part of the library name is its own subspace
        # e.g. std.types and std.math would share the std subspace
        strings = self.lexer.libName.split('.')
        lookup = self.libraries
        for s in strings:
            # Create new subspace
            if s not in lookup:
                lookup[s] = {}

            # Step down
            lookup = lookup[s]

        # Nothing has been resolved for this library yet
        self.resolvedObjects[self.lexer.libName] = {}

    def end_library(self):
        '''End collecting dependencies for the current library.'''
        self.resolve()

    def resolve(self):
        '''Resolve all dependencies.'''
        if self.unresolvedDependencies:
            # Traverse from top to bottom
            unresolvedDependencies = deque(self.unresolvedDependencies)
            
            # Clear the unresolved dependencies, they are in the queue
            self.unresolvedDependencies = {}
            
            while unresolvedDependencies:
                dependency = unresolvedDependencies.popleft()

                # The name has to be resolvable by now
                # Catching unresolved objects will spawn templates, if encountered
                self.links[dependency] = self.navigate(dependency.obj.anchor, dependency.references, dependency.location)

                # Add the unresolved dependencies to the queue
                unresolvedDependencies += self.unresolvedDependencies

                # And clear the cache again
                self.unresolvedDependencies = {}

        # Try to resolve location conflicts now
        for conflict in self.locationConflicts:
            # Look at both conflicts
            first = conflict.firstDependency.location[-1].parameters
            second = conflict.secondDependency.location[-1].parameters

            # Lookup the resolved dependencies
            for i, p0 in enumerate(first):
                p1 = second[i]

                # TODO: Lookup link of parameter dependency
                # REPLACE FIRST AND SECOND WITH RESOLVED
                pass

            if first == second:
                pass

    def to_graph(self):
        pass

class ProjectDependencyGraph:
    pass