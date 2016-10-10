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
        objs = [(obj, None) for obj in self.rootNamespace.objects]
        unresolvedNodes = []
        while objs:
            tuple = objs.pop()
            obj = tuple[0]
            parentGuidStr = tuple[1]

            # Generate a GUID for the object
            guid = obj.guid()
            guidStr = str(guid)
            self.dependencies.add_node(guidStr, dependency=Dependency(obj))

            # Recursive objects (Namespaces, Functions)
            children = getattr(obj, "objects", None)
            objs += [(child, guidStr) for child in  children] if children is not None else []

            # Connect the dependencies
            if parentGuidStr is not None:
                self.dependencies.add_edge(parentGuidStr, guidStr)

            # Connect immediate dependencies
            if isinstance(obj, Instruction):
                # Nothing to do. The expressions are handled after everything else is resolved
                pass
            elif isinstance(obj, Function):
                # TODO: create unresolved typeref for every parameter type
                pass
            elif isinstance(obj, MemberList):
                # Handled above
                pass
            elif isinstance(obj, Alias):
                # TODO: create unresolved typeref for targetType
                pass
            elif isinstance(obj, Template):
                # Templates don't have members
                pass
            elif isinstance(obj, Struct):
                # Handled above
                pass
            elif isinstance(obj, Namespace):
                # Handled above
                pass
            else:
                assert False
                
        print("Nodes of graph: ")
        print(self.dependencies.nodes())
        nx.draw(self.dependencies, with_labels=True)
        plt.show()

    @staticmethod
    def merge(graphs):
        # Try to connect unresolved dependencies
        pass