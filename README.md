# ppx_typeclass

ppx_typeclass is to provide typeclass/modular implicit to OCaml via ppx framework.

## Highlights

* This is not a compiler patch, but a ppx plugin.  You can use typeclasses with the vainlla OCaml compiler.
* ppx_typeclass is a source-to-source converter and the result is type-checked by the vanilla OCaml compiler.

## Limitations

* This does not work with toplevel.
