Structures
==========
Structures represent chunks of data. However, these data chunks do not necessarily have to be mapped to physical data after compilation. In the symbolic language, they merely serve as a means to pass in external data to a function. The words *struct* or *structure* and *type* are used interchangably in the symbolic language.

Description
-----------
Functions represent data transforms which operate on structures. So, to pass in data to any function, the data has to be represented as a structure. They can hold an arbitrary number of *member lists*. A member list consists of multiple *members* which share a common type. The following example illustrates the declaration of a four-dimensional vector type :code:`float4`, which has one member list of type :code:`float` and four members:

.. code-block:: cpp

    struct float4
    {
        float x, y, z, w;
    }
  
This data can now be passed in to a function :code:`add` which adds two vectors together:

.. code-block:: cpp

    float4 add(float4 a, float4 b);

Constructors
------------
To initialize local variables within a function body, a new function can be declared, whose return type is the desired variable type. By default, the symbolic compiler will generate a *default constructor* function, when a new type is declared, whose parameter list is deduced from the members of the structure. The :code:`float4` type declaration above will generate the following default constructor:

.. code-block:: cpp

    float4 float4(float x, float y, float z, float w);
    
If no default constructor is required, the :code:`[noconstructor]` annotation can be used on the type. Likewise, if a member list should not be part of the default constructors parameter list, the member list can be annotated with the :code:`[uninitialized]` annotation. The :code:`[uninitialized]` annotation has no effect, if the :code:`[noconstructor]` annotation is specified.
