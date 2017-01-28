Templates
=====================
Templates are constructs that enable generic programming in the symbolic language. They can reduce code redundancy by providing
expressiveness for more general concepts.

Problem
---------------------
The symbolic language is strongly typed and therefore requires types to be explicitly stated when referenced, e.g. in function
signatures. This can lead to duplicate code and hence redundancy in the code. The following example illustrates the problem:

.. code-block:: cpp

    import hlsl;
    
    int add(int a, int b)
    {
        return a + b;
    }
    
    float add(float a, float b)
    {
        return a + b;
    }
    
    void f()
    {
        add(1, 2);
        add(1.0, 2.0);
    }
    
The two definitions of :code:`add` look similar and differ solely by the types that are passed in.

Solution
---------------------
*Templates* can be used to alleviate this redundancy which can substantially reduce the amount of manual programming required.

.. code-block:: cpp

    import hlsl;
    
    template<T>
    T add(T a, T b)
    {
        return a + b;
    }
    
    void f()
    {
        add(1, 2);
        add(1.0, 2.0);
    }
    
A single template can be used to handle both cases by substituting the correct types for the signature.
Templates behave similar to C macros, except that they emit new translation units into the translator when referenced instead of an
in-place transformation of the source code. Looking at the example above, the call to :code:`int add(int, int)` instantiates the
:code:`add` function template with :code:`T="int"`, resulting in the first definition of :code:`add` from the first example.
The next call instantiates the same template with :code:`T="float"`, resulting in the second definition of :code:`add` from
the first example. Notice that the operator reference :code:`+` is also affected by this, resolving the right operator for each
type :code:`T`.

Templates are first class citizens of the language and can be used with all language objects, namely functions, structures,
aliases and namespaces. A unified syntax for templates is used:

.. code-block:: cpp

    template<<T0[=<T0P>]>, <T1[=<T1P>]>, ..., <TN[=<TNP>]>>
    <TEMPLATE_OBJECT>
    {
        ...
    }
   
where :code:`<TEMPLATE_OBJECT>` is the object to be templated, :code:`<T0>, <T1>, ..., <TN>` are the *template parameters*
and the corresponding, optional values :code:`<T0P>, <T1P>, ..., <TNP>` are the *partial specialization masks*.
See    for more information on partial template specialization. 

.. role:: note_info

:note_info:`Template parameters are passed in as strings that are unpacked before substitution. This allows spaces and
token delimiters to be included in the parameters.`

Partial Specialization
----------------------
Partial specialization can be used to specialize a template when one or more of its parameters match a user-defined pattern.
This can be useful when certain template parameter values should be treated differently than others.
Partial specializations take precedence over their unspecialized counterparts and other partial specializations 
with less matches. See example 

Concatenation
-------------
Template parameter concatenation is a way of combining multiple template parameters into a single symbol.
See example 

Examples
--------
This section contains examples that illustrate different uses of templates.

Example 1 - Generic Structures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A common use-case is to create generic data structures. The following code snippet demonstrates this:

.. code-block:: cpp
    
    template<T0, T1>
    struct my_type
    {
        T0 a;
        T1 b;
    }
    
    do_stuff(ref my_type<"int", "float"> c)
    {
        c.a = 42;
        c.b = 0;
    }
    

Example 2 - Type Generation
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Templates can be used to generate new types. The following code snippet demonstrates this:

.. code-block:: cpp
    
    // Creates a nested type with 2 members.
    template<T0, T1>
    struct emit_type2
    {
        struct type
        {
            T0 a;
            T0 b;
        }
    }
    
    // Provide a nicer way to reference the template for integers.
    using emit_ints
    {
        // T1 will be substituted with int during instantiation.
        emit_type2<"T1 a; T1 b;", "int">
    }
    
    int add(emit_ints.type c)
    {
        return c.a + c.b;
    }

Example 3 - Partial Specialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example illustrates different use-cases of partial template specialization.