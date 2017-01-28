Namespaces
==========
Namespaces are containers for other objects types in the symbolic language.

Problem
---------------------
The symbolic language supports a variety of different objects, namely aliases, functions, other namespaces and structures.
Objects with the same name can lead to location clashes, if they are declared within the same scope. The following example
illustrates the problem:

.. code-block:: cpp

    import hlsl;

    // Add-operation on real numbers.
    float add(float a, float b)
    {
        return a + b;
    }

    // Add-operation on complex numbers with implicit imaginary part.
    Complex add(float a, float b)
    {
        return Complex(a + b, 0.0);
    }
	
Namespaces can be used as an alternative way of resolving this location conflict without changing the name of the function.

Solution
---------------------
The symbolic language uses the same namespace approach that many existing C-style languages employ to resolve the conflict above.
Namespaces can contain aliases, functions, other namespaces and structs.
The following example illustrates the use of namespaces to solve the problem above:

.. code-block:: cpp

    import hlsl;

    // Add-operation on real numbers.
    float add(float a, float b)
    {
        return a + b;
    }

    namespace Complex
    {
        // Add-operation on complex numbers with implicit imaginary part.
        Complex add(int a, int b)
        {
            return Complex(a + b, 0.0);
        }
    }
    
While not strictly necessary, namespaces provide a convenient way to resolve location conflicts and are first-class citizens in the symbolic language.
In fact, functions and structures are namespaces themselves, allowing them to contain other namespaces. This gives great flexibility to nest declarations and data definitions.

Examples
--------
This section contains an example to illustrate a use-case of namespaces.


Example - JSON-like Data Definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example illustrates how to use namespaces to nest data definitions. The following snippet looks a lot like JSON but is valid symbolic code.

.. code-block:: cpp

    // Required by the 3-parameter function declaration below.
    struct Tim;
    struct Jim;
    struct Jon;
    
    // A (anonymous, void, parameterless, function) declaration.
    // .. but it looks like a root namespace!
    {
        // A (void, parameterless, function) declaration.
        // .. but it looks like a nested definition!
        Jane
        {
            // A (void, parameterless, function) declaration.
            // .. but looks like a nested definition!
            Bob {}
        }
        // A (void, parameterless, function) declaration with user semantics.
        // .. but it looks like a key-value map!
        Ken : Jim
        {
            // A (void, 3-parameter, function) declaration.
            // .. but it looks like a list!
            (Tim, Jim, Jon);
        }
    }
    
Since functions are namespaces, the declaration above is valid symbolic code, but it looks very close to JSON.
This allows you to write simple data definitions right next to your regular code.