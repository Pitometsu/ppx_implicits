(*

  Pre-preprocessing for syntax sugars for 
    [%imp]
*)

open Ppxx.Helper

open Parsetree
open Asttypes
open Ast_mapper

(* [%imp ...] => (assert false) [@imp ...] *)
let extend super =
  let expr self e =
    match e.pexp_desc with
    | Pexp_extension ( ({txt="imp"} as strloc), x) ->
        { (Exp.assert_false ()) with
          pexp_loc = e.pexp_loc;
          pexp_attributes = [ (strloc, x) ] }
    | _ -> super.expr self e
  in
  { super with expr }
