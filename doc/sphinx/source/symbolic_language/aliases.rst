Aliases
=======
Aliases can be used to alias typenames. This can reduce the amount of code repetition by shortening complex typenames to a single symbol.

Problem
---------------------
Typenames can sometimes be very long and, in the case of templates, require the specification of template parameters. In these cases, it can be desirable to introduce a new typename to refer to an existing type. Considering the following program: 

.. code-block:: cpp

  template<T0>
  struct point;
  
  f(point<"int">);
  g(point<"int">);
  
it is clear, that we want to 
