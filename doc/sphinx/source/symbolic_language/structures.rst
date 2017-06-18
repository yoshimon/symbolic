Structures
==========
Structures represent chunks of data. However, these data chunks do not necessarily have to be mapped to physical data after compilation. In the symbolic language, they merely serve as a means to pass in external data to a function. The words *struct* or *structure* and *type* are used interchangably in the symbolic language.

Description
===========
Functions represent data transforms which operate on structures. So, to pass in data to any function, the data has to be represented as a structure. Structures are namespaces and can therefore contain other namespace objects. They can also contain special namespace objects  called *member lists*. A member list consists of multiple *members* which share a common type. The following example illustrates the declaration of a four-dimensional vector type :code:`float4`, which has one member list of type :code:`float` and four members:

.. code-block:: c

    struct float4
    {
        float x, y, z, w;
    }
  
This data can now be passed in to a function `add` which adds two vectors together:

.. code-block:: c

    float4 add(float4 a, float4 b);
