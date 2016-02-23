(* CR jfuruse: copied from Typpx.Make. Must be shared. *)

(* e [@embed e'] => e' *)

open Parsetree
open Ast_mapper

let extend super = 
  let expr self e = match e.pexp_attributes with
    | [ {txt="typpx_embed"}, PStr [ { pstr_desc= Pstr_eval (e, []) } ] ] -> e
    | _ -> super.expr self e
  in
  { super with expr }

let mapper = extend Ast_mapper.default_mapper

let unembed = mapper.expr mapper
