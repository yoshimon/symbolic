from symbolic.objects import *
import networkx as nx
import matplotlib.pyplot as plt

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

            # Generate a GUID for the object
            guid = obj.guid()
            guidStr = str(guid)
            self.dependencies.add_node(guidStr, dependency=Dependency(obj))

            # Recursive objects (Namespaces, Functions)
            children = getattr(obj, "objects", None)
            objs += children if children is not None else []

            # Connect immediate dependencies
            if isinstance(obj, Struct):
                pass
            if isinstance(obj, MemberList):
                pass
            elif isinstance(obj, Alias):
                pass
            elif isinstance(obj, Namespace):
                pass
            elif isinstance(obj, Template):
                pass
            elif isinstance(obj, Instruction):
                pass
            elif isinstance(obj, Function):
                pass
            else:
                assert False
                
        print("Nodes of graph: ")
        print(self.dependencies.nodes())
        nx.draw(self.dependencies)
        plt.show()

    @staticmethod
    def merge(graphs):
        # Try to connect unresolved dependencies
        pass