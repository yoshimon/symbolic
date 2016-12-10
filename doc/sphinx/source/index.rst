.. symbolic documentation master file, created by
   sphinx-quickstart on Sat Nov 19 00:13:43 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to symbolic's documentation!
====================================

symbolic is a flexible compiler front-end for creating custom, C-style Domain Specific Languages (DSL) that doesn't require any changes to cover a variety of target languages. This allows the target DSL to be cleanly separated from the lexing and parsing process. Any compatible, C-style DSL can be implemented using symbolic libraries and a custom compiler backend, that translates the symbolic intermediate representation (IR) to a target platform.

.. toctree::
    :maxdepth: 2
    :caption: Getting Started
    
    getting_started/project_configuration
    getting_started/compiling
    
.. toctree::
    :glob:
    :maxdepth: 2
    :caption: Modules
	
    modules/*

Glossary
========

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`