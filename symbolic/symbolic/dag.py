from symbolic.objects import *
import networkx as nx

class Dependency:
    def __init__(self, obj):
        # The dependency object (Function, Struct, etc)
        self.obj = obj
        self.target = None

        # Two system annotations are valid for all dependencies: private, deprecate
        self.isPrivate = Annotation.has('private', obj.sysAnnotations) if obj is isinstance(obj, Annotateable) else True
        self.isDeprecated = Annotation.has('deprecated', obj.sysAnnotations) if obj is isinstance(obj, Annotateable) else False

class UnitDependencyGraph:
    def __init__(self, references, rootNamespace):
        self.references = references
        self.rootNamespace = rootNamespace
        self.dependencies = nx.DiGraph()

        # Create a dependency for every object
        objs = list(self.rootNamespace.objects)
        while objs:
            obj = objs.pop()

            # The dependencies are resolved using symbolic matching only
            # Create a GUID for the object
            self.dependencies.add_node(obj, dependency=Dependency(obj))

            # Recursive objects (Namespaces, Functions)
            children = getattr(obj, "objects", None)
            objs += children if children is not None else []

            # Connect immediate dependencies
            if isinstance(obj, Struct):
                for member in obj.members:
                    pass
                
    @staticmethod
    def merge(graphs):
        # Try to connect unresolved dependencies
        pass