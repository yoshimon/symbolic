Structures
==========
Structures represent chunks of data. However, these data chunks do not necessarily have to be mapped to physical data after compilation. In the symbolic language, they merely serve as a means to pass in external data to a function. The words *struct* or *structure* and *type* are used interchangably in the symbolic language.

Description
-----------
Functions are transforms which operate on structures. They can hold an arbitrary number of *member lists*. A member list consists of multiple *members* which share a common type. The following example illustrates the declaration of a four-dimensional vector type :code:`float4`, which has one member list of type :code:`float` with four members:

.. code-block:: cpp

    struct float4
    {
        float x, y, z, w;
    }
  
This data can now be passed in to a function :code:`add` which adds two :code:`float4`-vectors together:

.. code-block:: cpp

    float4 add(float4 a, float4 b);

Constructors
------------
To initialize local variables within a function, a new function can be declared, whose return type is the desired variable type. By default, the symbolic compiler will generate a *default constructor* function, when a new type is declared, whose parameter list is deduced from the members of the structure. The :code:`float4` type declaration above will generate the following default constructor:

.. code-block:: cpp

    float4 float4(float x, float y, float z, float w);
    
If no default constructor is required, the :code:`[noconstructor]` annotation can be used for the structure.

.. code-block:: cpp

    [noconstructor]
    struct HasNoConstructor;

Likewise, if a member list should not be part of the default constructors parameter list, the member list can be annotated with the :code:`[uninitialized]` annotation.

.. code-block:: cpp

    struct MyStruct
    {
        float x, y;
        [uninitialized] float z;
        float w;
    }
    
    // Generated constructor: MyStruct MyStruct(float x, float y, float w);

The :code:`[uninitialized]` annotation has no effect, if the :code:`[noconstructor]` annotation is specified.

Properties
----------
Properties are functions which operate on a structure instance. Every property function receives an implicit :code:`ref T this` argument, where :code:`T` is the structure. This simplifies writing accessors to structure members:

.. code-block:: cpp

    struct MyStruct
    {
        float x, y;
        float sum() => this.x + y; // Explicit and implicit member reference.
    }
    
