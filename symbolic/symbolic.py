"""@package symbolic"""

"""The root namespace, containing the symbolic executable."""

# Built-in
import argparse
import datetime
import os

# Project
from symbolic.algorithm import Algorithm
from symbolic.project import Project
from symbolic.paths import VirtualPath

def main():
    """The main program entrypoint."""

    parser = argparse.ArgumentParser()
    parser.add_argument("path", help="The file path to the project configuration file.")
    parser.add_argument("--output", dest="outputFilePath", default="output.yml", help="The AST output file path.")
    parser.add_argument("--header", dest="showHeader", help="Print the program header before running the program.")
    args = parser.parse_args()

    # Info
    if args.showHeader:
        print("-" * 80)
        print("symbolic 0.Xdev")
        print("-" * 80)

    projConfigFilePath = VirtualPath(args.path)

    # Change the working directory to the project path
    os.chdir(str(projConfigFilePath.directory_path()))

    # Load and translate the project
    projectBuildStartTime = datetime.datetime.now()
    project = Project(projConfigFilePath)
    projectDependencyGraph = project.translate()
    #projectDependencyGraph.write_to_file(VirtualPath(args.outputFilePath))
    print()
    print("-" * 80)
    print("Project build successful. {0} elapsed.".format(Algorithm.dt_ms_string(projectBuildStartTime)))
    print("=" * 80)

if __name__ == "__main__":
    main()