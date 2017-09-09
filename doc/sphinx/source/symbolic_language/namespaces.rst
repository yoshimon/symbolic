Namespaces
==========
Namespaces are containers for other objects types in the symbolic language.

Problem
---------------------
Objects with the same name can lead to location clashes, if they are declared within the same scope. The following example
illustrates the problem:

.. code-block:: cpp

    float f(float a, float b)
    {
        // Do something...
	return 0.0;
    }
    
    int f(float a, float b)
    {
    	// Do something else.
        return 0;
    }
	
Namespaces can be used as an alternative way of resolving this location conflict without changing the name of the function.

Solution
---------------------
The symbolic language uses the same namespace approach that many existing C-style languages employ to resolve the conflict above.
Namespaces can contain aliases, functions, other namespaces and structs.
The following example illustrates the use of namespaces to resolve the location conflict above:

.. code-block:: cpp

    float f(float a, float b)
    {
        // Do something.
        return 0.0;
    }
    
    namespace AdditionalStuff
    {
        int f(float a, float b)
        {
	    // Do something else.
            return 0;
        }
    }
    
While not strictly necessary, namespaces provide a convenient way to resolve location conflicts without renaming an object.
