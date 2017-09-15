open Ppxx.Utils

let extend super =
  Pre_imp_spec.extend
  & Pre_typeclass.extend 
  & Pre_ext_imp.extend
      super
