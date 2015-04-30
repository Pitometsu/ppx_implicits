open Ppxx
open Parsetree
open Ast_mapper
open Location

(* [%imp Show]  => (assert false) [@imp Show]
   [%imp2 Show] => (assert false) [@imp2 Show]
   [%imp3]      => (assert false) [@imp3]
*)

let extend super =
  let expr self e =
    match e.pexp_desc with
    | Pexp_extension ( ({txt=("imp" | "imp2" | "imp3")} as strloc), x) ->
        { (Exp.assert_false ()) with
          pexp_loc = e.pexp_loc;
          pexp_attributes = [ (strloc, x) ] }
    | _ -> super.expr self e
  in
  { super with expr }

   
   
