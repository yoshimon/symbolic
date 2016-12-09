"""@package symbolic"""

"""The root namespace, containing the symbolic executable."""

# Built-in
import argparse
import os

# Project
from symbolic.project import Project
from symbolic.paths import VirtualPath

def main():
    """The main program entrypoint."""

    parser = argparse.ArgumentParser()
    parser.add_argument("path", help="The file path to the project configuration file.")
    parser.add_argument("--header", dest="showHeader", help="Print the program header before running the program.")
    args = parser.parse_args()

    # Info
    if args.showHeader:
        print("symbolic v0.9")

    projConfigFilePath = VirtualPath(args.path)

    # Change the working directory to the project path
    os.chdir(str(projConfigFilePath.directory_path().expanded()))

    # Load and translate the project
    project = Project(projConfigFilePath)
    project.translate()
    # TODO: save the translated project + libraries out

    print('Done.')

if __name__ == "__main__":
    main()