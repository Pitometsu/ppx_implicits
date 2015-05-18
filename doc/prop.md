# Typeful PPX and Value Implicits

## Abstract

This talk presents a novel preprocessing technique with types
using OCaml PPX preprocessing.
Normally PPX is an untyped AST preprocessor: target ASTs are not yet type-checked,
therefore it cannot perform type dependent preprocessing.
*Typeful PPX* is a PPX which firstly type-checks the input, then
works on the typed AST for type dependent behaviour.

`ppx_implicits` is presented as a demonstration of Typeful PPX.
This provides implicit values, whose definitions are automatically
given depending its type context by combining some recipe values.
Implicit values are extended to implicit parameters
using OCaml's optional parameters, then combining it with OCaml's first class module values,
it is almost trivial to have Modular Implicits and type classes.

