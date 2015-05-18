# Typeful PPX and Value Implicits

## Abstract

This talk presents a novel preprocessing technique with types
using OCaml PPX preprocession framework.
Normally PPX preprocessor converts ASTs which are not yet type-checked,
therefore it cannot perform type dependent preprocessing.
Typeful PPX is a PPX which firstly type-checks the input, then
works on the typed AST for type dependent behaviour.

`ppx_implicits` is presented as a demonstration of Typeful PPX.
This provides implicit values, whose definitions are automatically
given depending its type context by combining some recipe values.
Implicit values are easily extended to implicit parameters
using OCaml's optional parameters, then it is almost trivial to
have Modular Implicits and type classes combining it
with the first class module values.

