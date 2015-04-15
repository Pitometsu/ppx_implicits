# ppx_typeclass

ppx_typeclass is to provide typeclass/modular implicit to OCaml via ppx framework.

## Highlights

### Simple

ppx_typeclass is a pre-processor solution: **no need of compiler patching**. 
You can use typeclasses with the vainlla OCaml compiler now.

It only uses the result of the original type inference and does not modify it.

### Safety

The translation of ppx_typeclass does not use `Obj.magic` trick and it is type-checked again by the vanilla OCaml compiler.

## Limitations

* The use of optional arguments makes hard to provide non function overloading:
  `one : ?_d:(module Num with type a = 'a) -> 'a`
* This does not work with toplevel.
