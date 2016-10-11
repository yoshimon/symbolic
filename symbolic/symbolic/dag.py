from symbolic.objects import *
import networkx as nx
from networkx.drawing.nx_agraph import graphviz_layout
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
    def _create_dependency(self, unresolvedNodes, obj, guidObj, parentGuid):
        guid = str(guidObj.guid())
        self.dependencies.add_node(guid, dependency=Dependency(obj))
        self.dependencies.add_edge(parentGuid, guid)
        unresolvedNodes.append(guid)

    def __init__(self, references, rootNamespace):
        self.references = references
        self.rootNamespace = rootNamespace
        self.dependencies = nx.DiGraph()

        # Create a dependency for every object
        objs = [(obj, None) for obj in self.rootNamespace.objects]
        unresolvedNodes = []

        while objs:
            tuple = objs.pop()
            obj, parentGuid = tuple[0], tuple[1]

            # Generate a GUID for the object
            guid = str(obj.guid())
            self.dependencies.add_node(guid, dependency=Dependency(obj))

            # Recursive objects (Namespaces, Functions)
            children = getattr(obj, "objects", None)
            objs += [(child, guid) for child in children] if children is not None else []

            # Connect the dependencies
            if parentGuid is not None:
                self.dependencies.add_edge(parentGuid, guid)

            # Connect immediate dependencies
            if isinstance(obj, Function):
                # Return type
                self._create_dependency(unresolvedNodes, obj, obj.returnTypename, guid)

                # Parameters
                for parameter in obj.parameters:
                    self._create_dependency(unresolvedNodes, obj, parameter, guid)
            elif isinstance(obj, Alias):
                # Create unresolved typeref for targetType
                self._create_dependency(unresolvedNodes, obj, obj.targetTypename, guid)
            elif any(isinstance(obj, classType) for classType in [Instruction, MemberList, Alias, Template, Struct, Namespace]):
                pass
            else:
                assert False
                
        print("Nodes of graph: ")
        print(self.dependencies.nodes())
        layout = nx.spectral_layout(self.dependencies)
        nx.draw_networkx(self.dependencies, pos=layout, arrows=True, with_labels=True, node_size=10000)
        plt.show()

    @staticmethod
    def merge(graphs):
        # Try to connect unresolved dependencies
        pass