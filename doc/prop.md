# Typeful PPX and Value Implicits

## Abstract

This talk presents Typeful PPX, a novel PPX preprocessing technique with types.
Type dependent preprocessing is fairly easy once PPX is combined with
the compiler type checker.

`ppx_implicits` is presented as a demonstration of Typeful PPX,
which provides type dependent implicit values, combining it with
the optional parameters and the first class module values of OCaml,
it is easy for a PPX to have the same functionality as Modular Implicits
and type classes.

## Typeful PPX

CamlP4 and PPX are preprocessing frameworks of OCaml and they are now widely
used to provide new functionalities to
the language. For example, syntax extension (ex. `pa_monad` and `ppx_moandic`)
and automatic code generation (ex. `deriving` and `type_conv`).

They are extremely useful in the real world programming, but untyped:
both CamlP4 and PPX are preprocessors which work over `Parsetree`,
not-yet-typed ASTs. Even if someone gets a tiny but nifty idea of
type dependent program transformation, it was almost out of the scope
of CamlP4 and PPX and it had to be implemented as a compiler modification.
Compiler modification is a very subtle to be done correctly,
especially if it involves with typing. Distributing and installing
compiler modifications are also hard: even with the help of OPAM's
`opam switch`, many OCaml users do not consider to invest their time
to try your modifications. 

*Typeful PPX* is a technique to overcome this difficulty of OCaml language
enhancement with types. It does not preprocess the input, untyped AST
of `Parsetree` directly, but type-check it firstly and works on
the typed AST of `Typedtree` to make use of type annotations. 
Once the preprocessing of the typed AST is done, it untypes the result
to an untyped AST as the final output. From the point of view of
the host compiler which invokes a typeful PPX, it is just another ordinary
but rather complicated PPX which transforms untyped ASTs.

Typeful PPX has the following benefits compared with the direct compiler
modification.

Safe. The output of Typeful PPX is again type checked by the compiler.
Critical bugs in Typeful PPX should be found by this second type check.
In the direct compiler modification, bugs in the typing layer tend to
make the type system unafe and they are hard to detect and fix.
This should also make users feel much easier to try new functionalities
via Typeful PPX than the compiler patching. If still unsure, users can always
print out the final output and verify what Typeful PPX actually does. 

Easy to distribute, install and use. A Typeful PPX is just a PPX.
Typeful PPXs are easily installable via the packaging system and
users can use their functions with their vanilla OCaml compiler immediately.

Easy future integration. Implemented as a transformer of `Typedtree`,
future integration of Typeful PPXs into the real compiler modification
can reuse the much of their code.

Unfortunately it has some drawbacks too:

Upto typing layer: this is a typeful program transformation
and therefore cannot change the lower details like code generation.

Not working with toplevel (REPL): PPX preprocessing works
against each compilation unit, which is one toplevel expression in OCaml toplevel.
Typeful PPX usually must keep various type informations
across toplevel expressions therefore does not work in OCaml toplevel.


--------

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

