(*

  Pre-preprocessing for syntax sugars for [%imp] and [%%imp_policy].

*)

open Ppxx
open Parsetree
open Asttypes
open Ast_mapper
open Location
open Utils
open Compilerlibx
open List

(* %typeclass *)
module TypeClass = struct
  open Longident
  open Ast_helper

  let parameters sg =
    concat_map (fun si ->
      match si.psig_desc with
      | Psig_type tds ->
          filter_map (fun td ->
            match td with
            | { ptype_name = {txt}; ptype_params = []; ptype_cstrs = []; ptype_kind = Ptype_abstract; ptype_manifest = None; } -> Some txt
            | _ -> None) tds
      | _ -> []) sg

  (* type 'a s = (module Show with type a = 'a) *)
  let gen_s name ps =
    let tvars = map (Typ.var ?loc:None) ps in (* CR jfuruse: loc *)
    Type.mk ?loc:None
      ~params: (map (fun tv -> (tv, Invariant)) tvars)
      ~manifest: (Typ.package ?loc:None 
                   (at (Lident name)) 
                   (map2 (fun p tv -> (at (Lident p), tv)) ps tvars))
      (at "s") (* CR jfuruse: can have a ghost loc *)

  (* type ('a, 'b) t = Packed of ('a, 'b) s *)
  let gen_t ps =
    let tvars = map (Typ.var ?loc:None) ps in (* CR jfuruse: loc *)
    Type.mk ?loc:None
      ~params: (map (fun tv -> (tv, Invariant)) tvars)
      ~kind: (Ptype_variant [ Type.constructor 
                               ?loc:None 
                              ~args:[ Typ.constr ?loc:None
                                      (at ?loc:None (Lident "s"))
                                      tvars ]
                              (at ?loc:None "Pack") ])
      (at "t")

  let pack_unpack = 
    [%str module Instances = struct
            let pack ~_x = Packed _x
            let pack_opt ~_x = Some  (Packed _x)
          end
          let unpack_opt ?_imp = match _imp with
            | None -> assert false
            | Some (Packed x) -> x
    ] 

  (* [%%imp_policy opened ShowInstance] *)
  let policy name = 
    let opened = 
      [%expr opened [%e Exp.construct (at ?loc:None (Lident (name ^ "Instance"))) None ]] 
    in
    Str.extension ?loc:None (at "imp_policy", PStr [(Str.eval opened)])

  let process_module_type_declaration mtd =
    let name = mtd.pmtd_name.txt in
    match mtd.pmtd_type with 
    | Some { pmty_desc = Pmty_signature sg } -> 
        let ps = parameters sg in
        begin match ps with
        | [] -> assert false (* error. no parameters *)
        | _ -> 
            Str.type_ [gen_s name ps]
            :: Str.type_ [gen_t ps]
            :: policy name
            :: pack_unpack
        end
    | Some _ -> assert false (* error *)
    | None -> assert false (* error *)
end

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
    let mangled = Policy.to_mangled_string policy in                
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
  let has_typeclass_attr = function
    | {txt="typeclass"}, PStr [] -> true
    | {txt="typeclass"}, _ -> assert false (* CR jfuruse: error *)
    | _ -> false
  in
  let structure_item self sitem = 
    match sitem.pstr_desc with
    | Pstr_extension (({txt="imp_policy"; loc}, pld), _) ->
        let policy = Policy.(from_ok loc & from_payload pld) in
        if policy = Policy.Type then 
          errorf "%a: [%%%%imp_policy POLICY] requires a POLICY expression"
            Location.format loc;
        { sitem with pstr_desc = Pstr_type [ forge loc policy ] }
    | Pstr_modtype mtd when List.exists has_typeclass_attr mtd.pmtd_attributes ->
        super.structure_item self sitem
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

