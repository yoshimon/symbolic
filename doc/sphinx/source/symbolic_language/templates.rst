Templates
=====================
Templates are constructs that enable generic programming in the symbolic language. They can reduce code redundancy by providing
expressiveness for more general concepts.

Problem
---------------------
The symbolic language is statically typed and requires types to be explicitly stated when referenced, e.g. in function
signatures. This can lead to duplicate code and hence redundancy. The following example illustrates the problem:

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
    
Both versions of :code:`add` look similar and differ solely by the types that are passed in.

Solution
---------------------
*Templates* can be used to alleviate this redundancy which can substantially reduce the amount of manual programming required
to implement certain concepts.

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
    
A single template can be used to handle both cases by substituting the correct types for the signature after a symbolic matching pass.
Templates behave similar to macros in the C language, except that they emit new translation units into the translator when referenced
instead of an in-place transformation of the source code. Looking at the example above, the call to :code:`int add(int, int)`
instantiates the :code:`add` function template with :code:`T="int"`, resulting in the first definition of :code:`add` from the
first example. The next call instantiates the same template with :code:`T="float"`, resulting in the second definition of :code:`add`
from the first example. Notice that the operator reference :code:`+` is also affected by this, resolving the right operator for each
type :code:`T`.

Templates are an essential part of the symbolic language and can be used with all scoped language objects, namely aliases, functions,
namespaces and structs. A unified syntax for templates is introduced:

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
other delimiters to be included in the parameter values.`

Partial Specialization
----------------------
Partial specialization can be used to specialize a template when one or more of its parameters match a user-defined pattern.
This can be useful when certain template parameter values should be treated differently than others.
Partial specializations take precedence over their unspecialized counterparts and other partial specializations 
with less matches. See `Example 3 - Partial Specialization`_ which demonstrates partial template specialization.

Concatenation
-------------
Two tokens can be concatenated into a single token using the :code:`><` operator.
This operator is only defined within templates and is referred to as *eppocatenator*. See `Example 4 - Concatenation`_ which
demonstrates in-template concatenation.

Under the Hood
--------------
Templates can be best thought of as a built-in text replacement tool. When declaring a template, the actual template body
is not inspected by the translator. All tokens between the opening :code:`{` and closing :code:`}` of the template body will be
stored. Within the template body, every opening :code:`{` brace has to have a matching closing :code:`}` brace.
After parsing, no templates are instantiated or inspected unless referenced.

.. code-block:: cpp

    template
    {
        This is an unnamed (anonymous) template. This entire template body will
        not be inspected until the template is referenced somewhere. In this
        particular case, since this template is anonymous, it can never be
        referenced elsewhere.
    }

To find a particular template, its location has to be resolved. Fortunately, resolving templates is not more difficult than
navigating to any other location. That being said, the introduction of templates adds another parameter dimension at every
location which is given by the template parameters. Therefore, a location is now completely defined
by all of its relative locations where each relative location must specify its name, its parameters and its
template parameters. To resolve partial template specializations, the navigation phase must inspect all partial matches first,
in descending order of the number of template parameter matches. The following example illustrates how locations are derived from a given
piece of source code:

.. code-block:: cpp

    import hlsl;

    struct int;                   // 1. Location: int<>()

    struct f                      // 2. Location: f<>()
    {
        struct int;               // 3. Location: f<>().int<>()

        template<>                // 4. Location: f<>().g<>(int)
        g(int)
        {
            template<Param>       // 5. Location: f<>().g<>(int).h<T0>()
            struct h(Param);

            template<"int">       // 6. Location: f<>().g<>(int).h<T0="int">()
            struct h(int);        // Will resolve to f<>().int<>().

            struct i              // 7. Location: f<>().g<>(int).i<>()
            {
                h<"float">;       // Will resolve to f<>().g<>(int).h<T0>()
                h<"int">;         // Will resolve to f<>().g<>(int).h<T0="int">()
            }
        }
    }

As can be seen in the example, no two locations within the same library can be identical. Otherwise a location conflict will be reported
by the translator when declaring the conflicting object since the translator would not be able to unambiguously resolve the object
during navigation.

Once the template location has been resolved, it will be instantiated by copying the templated object into a new translation unit
within the same library that instantiated the template. The object will be anonymized before the copy, so that multiple locations
can refer to the same template without generating conflicting locations. The translator will keep track of an internal table that
reduces duplicate template instantiations. However, duplicate instantiations will only be detected if they match exactly, that is when
their template parameters and imported libraries are identical. The new translation unit will then be parsed and analyzed like every
other unit using the references at the instantiators site. Assuming we want to instantiate :code:`h<"float">` in the example above,
the new, hidden translation unit will look like this:

.. code-block:: cpp

    struct (float);

The translator will create a link to this anonymized object by mapping it to the template location so that it can be navigated to
internally.

During the lexing step, all template parameters will be substituted with their corresponding values. Whenever
a template parameter gets substituted, the translator will blacklist the substitution of that same parameter within the substituted
string to prevent an infinite substitution recursion.

.. code-block:: cpp

    template<T0>
    struct a_type
    {
        T0
    }

    f(a_type<"Some T0 Here">);

In the example above, the template parameter :code:`T0` will only be substituted once by :code:`Some T0 Here`. The :code:`T0`
token inside the substituted string will not be replaced so that the substitution phase ends after the first pass.

Examples
--------
This section contains examples that illustrate different uses of templates.

Example 1 - Generic Structures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A common use-case is to create generic data structures. The following code snippet demonstrates this:

.. code-block:: cpp

    import hlsl;
    
    template<First, Second>
    struct a_type
    {
        First a;
        Second b;
    }
    
    void f(ref a_type<"int", "float"> p)
    {
        p.a = 42;
        p.b = 0.0;
    }
    

Example 2 - Type Generation
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Templates can be used to generate new types. The following code snippet demonstrates this:

.. code-block:: cpp

    import hlsl;
    
    // Allow external injection of source code into this type.
    template<Type, Injection>
    struct generated
    {
        Type a;
        Injection
    }
    
    int add(generated<"T1 b; T1 c;", "int"> p)
    {
        return p.a + p.b + p.c;
    }

Example 3 - Partial Specialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example illustrates partial template specialization.

.. code-block:: cpp

    import hlsl;
    
    template<T0>
    struct a_type
    {
        T0 a;
    }

    template<T0="int">
    struct a_type
    {
        T0 a;
        T0 b;
    }

    // Resolved to a_type<T0>.
    int add(a_type<"float"> p)
    {
        return p.a;
    }

    // Resolved to a_type<T0="int">.
    int add(a_type<"float"> p)
    {
        return p.a + p.b;
    }

Example 4 - Concatenation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example illustrates partial template specialization.

.. code-block:: cpp

    import hlsl;

    template<T0, T1>
    struct a_type
    {
        T0 >< T1 a;
    }

    float4 f(a_type<"float", "4"> p)
    {
        return p.a;
    }