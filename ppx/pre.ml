open Ppxx.Utils

let extend super =
  Pre_imp_spec.extend
  & Pre_typeclass.extend super
