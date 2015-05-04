(* size of type *)
open Btype

let size ty = 
  let type_exprs = ref 0 in
  let it = 
    { type_iterators with
      it_do_type_expr = (fun it ty ->
        incr type_exprs;
        type_iterators.it_do_type_expr it ty)
    }
  in
  it.it_type_expr it ty;
  unmark_iterators.it_type_expr unmark_iterators ty;
  !type_exprs
