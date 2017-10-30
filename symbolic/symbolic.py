"""@package symbolic"""

"""The root namespace, containing the symbolic executable."""

# Built-in
import argparse
import datetime
import os
import sys

# Project
from symbolic.algorithm import Algorithm
from symbolic.project import Project
from symbolic.paths import VirtualPath
from symbolic.serializers import LinkedProjectYamlSerializer

def main():
    """The main program entrypoint."""
    parser = argparse.ArgumentParser()
    parser.add_argument("path", help="The file path to the project configuration file.")
    parser.add_argument("--typesoutput", dest="typesOutputFilePath", default="types", help="The output file path for the linked types.")
    parser.add_argument("--functionsoutput", dest="functionsOutputFilePath", default="functions", help="The output file path for the linked functions.")
    parser.add_argument("--serializer", dest="serializer", default="yaml", help="The output serializer to use.")
    parser.add_argument("-i", "--info", default=False, action="store_true", help="Print the program header before running the program.")
    parser.add_argument("-d", "--debug", default=False, action="store_true", help="Enable debug mode. This will print a traceback when an error occurs.")
    args = parser.parse_args()

    # Info
    if args.info:
        print("=" * 80)
        print("symbolic 0.Xdev")

    if not args.debug:
        sys.excepthook = lambda exctype, value, traceback: print("\n!> {0}: {1}".format(str(exctype.__name__), str(value)))

    projConfigFilePath = VirtualPath(args.path)

    os.chdir(str(projConfigFilePath.directory_path()))

    # Load and translate the project
    projectBuildStartTime = datetime.datetime.now()
    project = Project(projConfigFilePath)
    linkedProject = project.translate()
    print()
    print("-" * 80)
    print("(4) Output Generator")
    print("-" * 80)
    print("Serializing results...")
    LinkedProjectYamlSerializer.run(VirtualPath(args.typesOutputFilePath), VirtualPath(args.functionsOutputFilePath), linkedProject)
    print()
    print("Project build successful. {0} elapsed.".format(Algorithm.dt_ms_string(projectBuildStartTime)))
    print("=" * 80)

if __name__ == "__main__":
    main()