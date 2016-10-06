import sys
import json
import os
import glob
import io
import networkx as nx
from jinja2 import Environment

from symbolic.parsers import UnitParser
from symbolic.lexer import SymbolicLexer, Symto
from symbolic.dag import UnitDependencyGraph

# Loads a pre-processor table from file
def load_ppt(filePath):
    try:
        with open(filePath) as filePath:
            jsonFile = json.load(filePath)
            return jsonFile['preprocessor']
    except:
        return {}

# Merges pre-processor tables
def merge_ppts(tables):
    result = {}
    for t in tables:
        for p in t:
            for key, val in p.items():
                result[key] = val

    return result

if __name__ == "__main__":
    # Info
    print('Symbolic (C) 2016, WOZ')
    print('Version 1.0.0.0 (DEBUG)')

    # Make sure we have a JSON filename for the project
    if len(sys.argv) < 2:
        sys.exit('Missing project filename.')

    # Grab args
    projFilePath = sys.argv[1]

    # Get the base project directory
    projDirPath = os.path.dirname(projFilePath)

    # Setup the default Jinja environment
    jinjaEnv = Environment(
        block_start_string='{%',
        block_end_string='%}',
        variable_start_string='{{',
        variable_end_string='}}',
        comment_start_string='/*',
        comment_end_string='*/',
        line_statement_prefix='#',
        line_comment_prefix='//'
    )
    
    # Bind the pre-processor to the lexer
    lexer = SymbolicLexer(jinjaEnv)

    # Create a dependency graph to sort the libraries
    libDepGraph = nx.DiGraph()

    # Read & parse the project JSON descriptor
    with open(projFilePath) as jsonProjFileData:
        jsonProjFile = json.load(jsonProjFileData)

        # Create project pre-processor table
        projectPPT = load_ppt(projFilePath + '.pp')

        # Load all library descriptors
        for p in jsonProjFile['project']['libraries']:
            relLibPath = p['library']

            # Combine to get the absolute library path
            absLibPath = os.path.normpath(os.path.join(projDirPath, relLibPath))

            # Extract the library name
            libName = os.path.basename(absLibPath)

            print('Loading library "{:s}"...'.format(libName))

            # There has to be 1 manifest
            libManifestFilePath = os.path.join(absLibPath, '.manifest.json')

            # Create a node for this library
            libDepGraph.add_node(libName, absLibPath=absLibPath)

            # Load the manifest
            try:
                with open(libManifestFilePath) as jsonManFileData:
                    jsonManFile = json.load(jsonManFileData)

                    # Connect the node to all dependencies
                    for ref in jsonManFile['library']['references']:
                        libDepGraph.add_edge(libName, ref['reference'])
            except:
                # No manifest, ignore this
                pass

        print('Resolving library dependencies...')

        # Get a linearized dependency graph
        try:
            libDepSortedGraph = nx.topological_sort(libDepGraph)
        except nx.exception.NetworkXUnfeasible:
            libDepCycle = nx.find_cycle(libDepGraph)

            # Composite dependency
            dependencyChain = ''
            for p in libDepCycle:
                dependencyChain += '{:s} -> '.format(p[0])
            dependencyChain += p[1]

            sys.exit('Library dependency cycle found: [{:s}].'.format(dependencyChain))

        # Translate each library by going through all *.sym files
        for node in libDepGraph.nodes(data=True):
            # Update lexer state
            lexer.libName = node[0]

            absLibPath = node[1]['absLibPath']

            # Load the per-library pre-processor table
            libPPT = load_ppt(absLibPath + '\\.lib.pp')

            libFileSearchMask = os.path.join(absLibPath, '*.sym')
            for filePath in glob.iglob(libFileSearchMask, recursive=True):
                print('Parsing "{:s}"...'.format(filePath))

                # Load the per-file pre-processor table
                filePPT = load_ppt(filePath + '.pp')

                # Merge all pre-processor tables by overwriting
                lexer.ppt = merge_ppts([projectPPT, libPPT, filePPT])

                # Load the source file
                with open(filePath, 'r') as srcFile:
                    lexer.fileName = filePath
                    srcFileTokens = lexer.tokenize(srcFile.read())
                    
                    # Parse the unit and extract an object representation
                    unitParser = UnitParser(lexer, srcFileTokens)
                    references, globalNamespace = unitParser.parse()

                    # Create a dependency graph for the unit
                    unitDAG = UnitDependencyGraph(references, globalNamespace)

                    # Dump the sorted DAG to disk
                    
                    # TODO: if an error occurs: show the expanded context

        print('Done.')