(*

  Pre-preprocessing for syntax sugars for [%imp] and [%%imp_policy].

*)

open Utils
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
  let forge loc policy policy_loc =
    let mangled = Policy.(mangle & to_string policy) in                
          { ptype_name = {txt = "__imp_policy__"; loc}
          ; ptype_params = []
          ; ptype_cstrs = []
          ; ptype_kind = Ptype_variant [
            { pcd_name = {txt=mangled; loc=policy_loc}
            ; pcd_args = []
            ; pcd_res = None
            ; pcd_loc = policy_loc
            ; pcd_attributes = []
            }
          ]
          ; ptype_private = Private (* How dare you want to use it? *)
          ; ptype_manifest = None
          ; ptype_attributes = []
          ; ptype_loc= loc
          }
  in
  let structure_item self sitem = 
    match sitem.pstr_desc with
    | Pstr_extension (({txt="imp_policy"; loc}, pld), _) ->
        let policy, policy_loc = Policy.from_payload pld in
        { sitem with pstr_desc = Pstr_type [ forge loc policy policy_loc ] }
    | _ -> super.structure_item self sitem
  in
  let signature_item self sitem = 
    match sitem.psig_desc with
    | Psig_extension (({txt="imp_policy"; loc}, pld), _) ->
        let policy, policy_loc = Policy.from_payload pld in
        { sitem with psig_desc = Psig_type [ forge loc policy policy_loc ] }
    | _ -> super.signature_item self sitem
  in
  { super with expr; structure_item; signature_item }

(*
  [%imp Show || Show2]
  [%imp Opened Show || Opened Show2]
*)



   
   
