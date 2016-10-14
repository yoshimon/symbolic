from symbolic.objects import *
from symbolic.language import *
from collections import deque
import networkx as nx
import matplotlib.pyplot as plt

class Dependency:
    def __init__(self, references, obj):
        self.obj = obj
        self.references = references

        # Two system annotations are valid for all dependencies: private, deprecate
        self.isPrivate = Annotation.has('private', obj.sysAnnotations) if obj is isinstance(obj, Annotateable) else True
        self.isDeprecated = Annotation.has('deprecated', obj.sysAnnotations) if obj is isinstance(obj, Annotateable) else False

class ProjectDependencyCollection:
    def __init__(self):
        self.libName = ''
        self.unresolvedDependencies = set()
        self.resolvedObjects = {}

    def navigate(self, scopeStrings):
        lookup = self.resolvedObjects
        for ns in scopeStrings:
            # The namespace has to exist
            if ns not in lookup:
                raise EOFError()
                
            # Step down this namespace    
            lookup = lookup[ns][1]

        return lookup

    def insert(self, references, obj):
        dependency = Dependency(references, obj)
        if obj.guid.kind == GUIDKind.Unresolved:
            # Only register, if not a native type
            if obj.guid.name not in Language.systemTypenameStrings:
                self.unresolvedDependencies.add(dependency)
        else:
            lookup = self.navigate(obj.guid.namespaceStrings)

            # Make sure that no duplicate object in this namespace exists
            if obj.guid.name in lookup:
                raise EOFError()

            lookup[obj.guid.name] = (dependency, {})

    def insert_unit(self, references, rootNamespace):
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

    def begin_library(self, libName):
        self.libName = libName

    def end_library(self):
        print(self.unresolvedDependencies)
        self.resolve()

    def resolve(self):
        if self.unresolvedDependencies:
            # Traverse from top to bottom
            unresolvedDependencies = deque(self.unresolvedDependencies)
            while unresolvedDependencies:
                dependency = unresolvedDependencies.popleft()

                # Expand tuple
                dependencies, objects = tuple[0], tuple[1] 

                # The name has to be resolvable by now
                match = self.navigate(path)
            
                # Register link to entry in resolvedObjects via linkedObjects

                # Go wide!


            # Everything was resolved
            self.unresolvedDependencies = {}

    def to_graph(self):
        pass

class ProjectDependencyGraph:
    pass