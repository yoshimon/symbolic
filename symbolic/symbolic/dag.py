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

class ProjectDependencyCollection:
    def __init__(self, lexer):
        '''Initialize the object.'''
        self.lexer = lexer
        self.unresolvedDependencies = set()
        self.libraries = {} # The libraries lookup table
        self.resolvedObjects = {} # Maps each library to a list of resolved objects
        self.links = {} # Maps unresolved dependencies to their resolved counterparts

    def navigate(self, errorToken, references, path):
        '''
        Navigate to a location.

        Args:
            references (list(Reference)): The library references.
            path (list(RelativeLocation)): The path.
            unresolvedTemplateDependencies (deque(Dependency)): Unresolved dependencies due to template instantiation.
        Returns:
            dict: The resolved location.
        '''
        # Navigate the library tree first (explicit library name)
        # Fall back to reference order in unit, if no match possible
        offset = -1 # The offset where the object name begins
        lookup = self.libraries
        for i, rl in enumerate(path):
            if rl.name in lookup:
                lookup = lookup[rl.name]
                offset = i
            else:
                # The rest is the object name
                break

        # Implicit lookup
        libNameGen = chain((ref.token.text for ref in references), [self.lexer.libName]) if offset == -1 else ['.'.join(path[:offset].name)]
        offset = max(0, offset)

        # Iterate to the object in the library
        for libName in libNameGen:
            lookup = self.resolvedObjects[libName]
            for i, rl in enumerate(path[offset:]):
                # The namespace has to exist
                if rl.name not in lookup:
                    raise UnknownIdentifierError(errorToken)

                # The dependencies associated with this name
                tup = lookup[rl.name]
                dependencies = tup[0]

                # ADL (parameter signature)
                dependency = next((d for d in dependencies if len(rl.parameterSignature) == len(d.location.path[i].parameterSignature)), None)

                if not dependency:
                    raise OverloadNotFoundError(errorToken)

                dependencyRL = dependency.location.path[i]

                # Make sure the template arguments match
                if len(rl.templateParameters) != len(dependencyRL.templateParameters):
                    raise TemplateMismatchError(errorToken)

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
                lookup = tup[1]

        return lookup

    def insert(self, references, obj):
        '''
        Insert an object into the dependency collection.

        Args:
            references (list(Reference)): The references list.
            obj (object): The object to insert.
        '''
        # Create and cache the dependency
        dependency = Dependency(references, obj)
        dependencyPath = dependency.location.path
        rl = dependencyPath[-1]

        if rl.kind == LocationKind.Unresolved:
            # Register, if not a native type
            if rl.name not in Language.systemTypenameStrings:
                self.unresolvedDependencies.add(dependency)
        else:
            # Navigate to parent
            lookup = self.navigate(obj.token, references, dependencyPath[:-1])

            # Make sure that no duplicate object in this namespace exists
            newState = ([dependency], {})
            if rl.name in lookup:
                # See if parameter signature and template parameter count matches
                for i, parameter in enumerate(rl.templateParameters):
                    raise EOFError()

                # Combine states
                oldState = lookup[rl.name]
                newState = (oldState[0] + newState[0], oldState[1])

            lookup[rl.name] = newState

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
            if s not in lookup:
                lookup[s] = {}

            # Step down
            lookup = lookup[s]

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
                self.links[dependency] = self.navigate(dependency.obj.token, dependency.references, dependency.location.path)

                # Add the unresolved dependencies to the queue
                unresolvedDependencies += self.unresolvedDependencies

                # And clear the cache again
                self.unresolvedDependencies = {}

    def to_graph(self):
        pass

class ProjectDependencyGraph:
    pass