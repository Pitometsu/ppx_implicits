(*

  Pre-preprocessing for syntax sugars for [%imp] and [%%imp_policy].

*)

open Ppxx
open Parsetree
open Ast_mapper
open Location
open Utils
open Compilerlibx

let extend super =
  let expr self e =
    match e.pexp_desc with
    | Pexp_extension ( ({txt="imp"} as strloc), x) ->
        { (Exp.assert_false ()) with
          pexp_loc = e.pexp_loc;
          pexp_attributes = [ (strloc, x) ] }
    | _ -> super.expr self e
  in
  let forge loc policy =
    let mangled = Policy.(mangle & to_string policy) in                
          { ptype_name = {txt = "__imp_policy__"; loc}
          ; ptype_params = []
          ; ptype_cstrs = []
          ; ptype_kind = Ptype_variant [
            { pcd_name = {txt=mangled; loc}
            ; pcd_args = []
            ; pcd_res = None
            ; pcd_loc = loc
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
        let policy = Policy.(from_ok loc & from_payload pld) in
        if policy = Policy.Type then 
          errorf "%a: [%%%%imp_policy POLICY] requires a POLICY expression"
            Location.format loc;
        { sitem with pstr_desc = Pstr_type [ forge loc policy ] }
    | _ -> super.structure_item self sitem
  in
  let signature_item self sitem = 
    match sitem.psig_desc with
    | Psig_extension (({txt="imp_policy"; loc}, pld), _) ->
        let policy = Policy.(from_ok loc & from_payload pld) in
        if policy = Policy.Type then 
          errorf "%a: [%%%%imp_policy POLICY] requires a POLICY expression"
            Location.format loc;
        { sitem with psig_desc = Psig_type [ forge loc policy ] }
    | _ -> super.signature_item self sitem
  in
  { super with expr; structure_item; signature_item }

(*
  [%imp Show || Show2]
  [%imp Opened Show || Opened Show2]
*)



   
   
