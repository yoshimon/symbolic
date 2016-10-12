from symbolic.objects import *
from symbolic.language import *
import networkx as nx
from networkx.drawing.nx_agraph import graphviz_layout
import matplotlib.pyplot as plt

class Dependency:
    def __init__(self, references, obj):
        self.references = references
        self.obj = obj
        self.target = None

        # Two system annotations are valid for all dependencies: private, deprecate
        self.isPrivate = Annotation.has('private', obj.sysAnnotations) if obj is isinstance(obj, Annotateable) else True
        self.isDeprecated = Annotation.has('deprecated', obj.sysAnnotations) if obj is isinstance(obj, Annotateable) else False

class LibraryDependencyGraph:
    def __init__(self):
        self.unresolvedNodes = set()
        self.dependencies = nx.DiGraph()

    def insert_unit(self, references, rootNamespace):
        # Create a dependency for every object
        objs = [(obj, None) for obj in rootNamespace.objects]
        while objs:
            tuple = objs.pop()
            obj, parentGuid = tuple[0], tuple[1]

            # Create a node and connect the parent -> child dependency
            guid = self.__insert_dependency(references, obj, obj, parentGuid)

            # Recursive objects (Namespaces, Functions)
            children = getattr(obj, "objects", None)
            objs += ((child, guid) for child in children) if children is not None else []

            # Connect immediate dependencies
            if isinstance(obj, Function):
                # Return type
                self.__insert_dependency(references, obj, obj.returnTypename, guid)

                # Parameters
                for parameter in obj.parameters:
                    self.__insert_dependency(references, obj, parameter, guid)
            elif isinstance(obj, Alias):
                # Create unresolved typeref for targetType
                self.__insert_dependency(references, obj, obj.targetTypename, guid)

    def dump(self):
        print("Nodes of graph: ")
        print(self.dependencies.nodes())
        layout = nx.spring_layout(self.dependencies)
        nx.draw_networkx(self.dependencies, pos=layout, arrows=True, with_labels=True, node_size=100)
        plt.show()

    def __insert_dependency(self, references, obj, guidObj, parentGuid):
        guid = guidObj.guid()
        guidStr = str(guid)

        # Add the dependency node
        if guid.name not in Language.systemTypenameStrings:
            self.__add_node(references, guidStr, obj)

            # Register it as unresolved
            if guid.kind == GUIDKind.Unresolved:
                self.unresolvedNodes.add(guidStr)

        isValidDependency = self.dependencies.has_node(guidStr)

        # Connect the parent to the node
        if (parentGuid is not None) and (isValidDependency):
            self.__add_edge(parentGuid, guidStr)

        return guidStr if isValidDependency else None

    def __add_edge(self, parentGuid, guid):
        self.dependencies.add_edge(parentGuid, guid)

    def __add_node(self, references, guid, obj):
        # Make sure that no node with the same scope exists

        self.dependencies.add_node(guid, dependency=Dependency(references, obj))