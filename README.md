# ppx_typeclass

ppx_typeclass is to provide typeclass/modular implicit to OCaml via ppx framework.

## Highlights

* No need of compiler patching: this is not a compiler patch, but a ppx plugin.  You can use typeclasses with the vainlla OCaml compiler.
* Assured type safety: the output of ppx_typeclass is type-checked by the vanilla OCaml compiler.

## Limitations

* This does not work with toplevel.