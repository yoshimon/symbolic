Aliases
=======
Aliases can be used to refer to an existing typename using a shortcut. This can reduce the amount of code repetition by shortening complex typenames to a single symbol.

Problem
-------
Typenames can sometimes be very long and, in the case of templates, require the specification of template parameters. In these cases, it can be desirable to introduce a new typename to refer to an existing type. Considering the following program: 

.. code-block:: cpp

    template<Type, Dimension>
    struct vector
    {
        Type >< Dimension data;
    }

    void f(vector<float, 4>);
    void g(vector<float, 4>);
  
The program declares two functions, :code:`f` and :code:`g`, which both accept a four-dimensional :code:`vector` of type :code:`float`. The full typename with all template parameters must be written out in every use case of the :code:`vector` type like this. This can lead to very long typenames for common types, especially when the number of template arguments grows.

Solution
--------
Instead of having to write :code:`vector<float, 4>` for every instance of such common constructs as four-dimensional vectors, aliases can be used to shorten the call-site overhead in the example above:

.. code-block:: cpp

    template<Type, Dimension>
    struct vector
    {
        Type >< Dimension data;
    }
    
    using v4f { vector<float, 4> }

    void f(v4f);
    void g(v4f);

Aliases can be templated too. In the example above, a :code:`vector4` can be templated like this:

.. code-block:: cpp

    template<T>
    using vector4 { vector<T, 4> }
    
    void f(vector4<float>);
    void g(vector4<float>);
