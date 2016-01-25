(*

  Pre-preprocessing for syntax sugars for 
    [%%imp_spec]
*)

open Ppxx.Utils
open Ppxx.Helper
open Ppxx.Compilerlib

open Parsetree
open Asttypes
open Ast_mapper
open List

(* type __imp_spec__ = private Spec_xxxx *)
let extend super =
  let forge_spec loc spec =
    let mangled, ctys = Specconv.mangle spec in
    let tvars = Utils.tvars_of_core_type & Typ.tuple ctys in
    Type.mk ~loc
      ~params: (map (fun x -> (Typ.var x, Invariant)) tvars)
      ~kind: (Ptype_variant [ { pcd_name = {txt=mangled; loc}
                              ; pcd_args = ctys
                              ; pcd_res = None
                              ; pcd_loc = loc
                              ; pcd_attributes = []
                              } ])
      ~priv: Private
      {txt = "__imp_spec__"; loc}
  in

  let do_imp_spec loc pld f = match Specconv.from_payload Env.empty (* dummy *) pld with
    | `Error err -> Specconv.error loc err
    | `Ok Spec.Type ->
        errorf "%a: [%%%%imp_spec SPEC] requires a SPEC expression"
          Location.format loc
    | `Ok spec -> f spec
  in

  let structure_item self sitem = 
    match sitem.pstr_desc with
    | Pstr_extension (({txt="imp_spec"; loc}, pld), _) ->
        (* [%%imp_spec ..] => type __imp_spec__ = .. *)
        do_imp_spec loc pld & fun spec -> 
          { sitem with pstr_desc = Pstr_type [ forge_spec loc spec ] }
    | _ -> super.structure_item self sitem
  in
  let signature_item self sitem = 
    match sitem.psig_desc with
    | Psig_extension (({txt="imp_spec"; loc}, pld), _) ->
        (* [%%imp_spec ..] => type __imp_spec__ = .. *)
        do_imp_spec loc pld & fun spec -> 
          { sitem with psig_desc = Psig_type [ forge_spec loc spec ] }
    | _ -> super.signature_item self sitem
  in
  { super with structure_item; signature_item }
