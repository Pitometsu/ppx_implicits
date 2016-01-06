(*

  Pre-preprocessing for syntax sugars for 
    [%imp]
    [%%imp_spec]
    [@@typeclass]
    [@@instance]
*)

open Ppxx.Utils
open Ppxx.Helper
open Ppxx.Compilerlib

open Parsetree
open Asttypes
open Ast_mapper
open List

(* [@@typeclass] and [@@instance] *)
module TypeClass = struct
  open Longident

  (* module type Show = sig type a val show : a -> string end
     => [a]
  *)
  let parameters sg = sort compare & concat_map (fun si ->
    match si.psig_desc with
    | Psig_type tds ->
        filter_map (fun td ->
          match td with
          | { ptype_name = {txt}; ptype_params = []; ptype_cstrs = []; ptype_kind = Ptype_abstract; ptype_manifest = None; } -> Some txt
          | _ -> None) tds
    | _ -> []) sg

  (* module type Show = sig type a val show : a -> string end
     => ["show", a -> string]
  *)
  let values sg = filter_map (fun si ->
    match si.psig_desc with
      | Psig_value vdesc -> Some (vdesc.pval_name.txt, vdesc.pval_type)
      | _ -> None) sg

  let add_newtypes = fold_right (fun s -> Exp.newtype ?loc:None s)

  let spec = [%stri [%%imp_spec typeclass] ]

  (* type 'a _module = (module Show with type a = 'a) *)
  let gen_ty_module name ps =
    let tvars = map (Typ.var ?loc:None) ps in (* CR jfuruse: loc *)
    Type.mk ?loc:None
      ~params: (map (fun tv -> (tv, Invariant)) tvars)
      ~manifest: (Typ.package ?loc:None 
                   (at (Lident name)) 
                   (map2 (fun p tv -> (at (Lident p), tv)) ps tvars))
      (at "_module") (* CR jfuruse: can have a ghost loc *)

  (* type ('a, 'b) _class = Packed of ('a, 'b) _module *)
  let gen_ty_class ps =
    let tvars = map (Typ.var ?loc:None) ps in (* CR jfuruse: loc *)
    Type.mk ?loc:None
      ~params: (map (fun tv -> (tv, Invariant)) tvars)
      ~kind: (Ptype_variant [ Type.constructor 
                               ?loc:None 
                               ~args:[ Typ.constr ?loc:None
                                       (at ?loc:None (Lident "_module"))
                                       tvars ]
                              (at ?loc:None "Packed") ])
      (at "_class")

  let pack_unpack = 
    [%str module Instances = struct
            let pack ~_x = Packed _x
            let pack_opt ~_x = Some  (Packed _x)
          end
          let unpack_opt ?_imp = match _imp with
            | None -> assert false
            | Some (Packed x) -> x
    ] 

  (* let show (type a) ?_imp = let module M = (val (unpack_opt ?_imp : a s)) in M.show *)
  let method_ tys (n,_ty) = 
    let paramed_s = 
      let open Typ in
      constr (at ?loc:None & Lident "_module") 
      & map (fun ty -> constr ?loc:None (at ?loc:None & Lident ty) []) tys
    in
    [%stri let [%p Pat.var' ?loc:None n] =
        [%e add_newtypes tys 
            [%expr fun ?_imp -> 
                     let module M = (val (unpack_opt ?_imp : [%t paramed_s]))
                     in [%e Exp.(ident ?loc:None (at ?loc:None (Ldot (Lident "M", n)))) ] ] ] ]

  let process_module_type_declaration mtd =
    let name = mtd.pmtd_name.txt in
    match mtd.pmtd_type with 
    | Some { pmty_desc = Pmty_signature sg } -> 
        let ps = parameters sg in
        let vs = values sg in
        begin match ps with
        | [] -> assert false (* error. no parameters *)
        | _ -> 
            Str.module_ ?loc:None & Mb.mk ?loc:None (at ?loc:None name)
              (Mod.structure ?loc:None 
               & [%stri [@@@warning "-16"]]
                 :: Str.type_ [gen_ty_module name ps]
                 :: Str.type_ [gen_ty_class ps]
                 :: spec
                 :: pack_unpack
                 @  map (method_ ps) vs
              )
        end
    | Some _ -> assert false (* error *)
    | None -> assert false (* error *)


  let parameters str =
    sort (fun (x,_) (y,_) -> compare x y) & concat_map (fun si ->
      match si.pstr_desc with
      | Pstr_type tds ->
          filter_map (fun td ->
            match td with
            | { ptype_name = {txt}; ptype_params = []; ptype_cstrs = []; ptype_kind = Ptype_abstract; ptype_manifest = Some ty; } -> Some (txt, ty)
            | _ -> None) tds
      | _ -> []) str

  (* 
     module ShowInt = struct
       type a = int
       let show  = string_of_int
     end [@@instance Show] 

     =>

     module ShowIntInstance = struct
       let dict : ShowInt.a Show.s = (module ShowInt)
       type __imp_instance__ = Show.__imp_spec__
     end
  *)
  let instance lid mb ~instance_loc =  
    let cname = match lid with (* Show *)
      | Lident cname -> cname
      | Ldot (_, cname) -> cname
      | _ -> assert false (* CR jfuruse: error *)
    in
    let iname = mb.pmb_name.txt in (* ShowInt *)
    let oname = iname ^ "Instance" in (* ShowIntInstance *)
    let str =
      let rec get_str me = match me.pmod_desc with
      | Pmod_structure str -> str
      | Pmod_constraint (me, _) -> get_str me
      | _ -> assert false (* CR jfuruse: error handling *)
      in
      get_str mb.pmb_expr
    in
    let ps = parameters str in
    with_gloc mb.pmb_loc & fun () ->
      Str.module_ & Mb.mk (at ~loc:mb.pmb_name.loc oname)
      & Mod.structure ~loc:mb.pmb_expr.pmod_loc
        [ Str.value ?loc:None Nonrecursive 
            [ Vb.mk ?loc:None 
                (Pat.var' ?loc:None "dict")
                (Exp.constraint_ ?loc:None 
                   (Exp.pack ?loc:None & Mod.ident' ?loc:None & Lident iname)
                   (Typ.constr ?loc:None 
                     (at ?loc:None & Ldot (lid, "_module"))
                    & map (fun (p,_) -> Typ.constr ?loc:None (at ?loc:None & Ldot (Lident iname, p)) []) ps))
            ]
        ; with_gloc instance_loc & fun () ->
          Str.type_ [ Type.mk ~manifest:(Typ.constr (at & Ldot (Lident cname, "__imp_spec__")) []) (at "__imp_instance__") ]
        ]
end

let extend super =
  let expr self e =
    match e.pexp_desc with
    | Pexp_extension ( ({txt="imp"} as strloc), x) ->
        (* [%imp ...] => (assert false) [@imp ...] *)
        { (Exp.assert_false ()) with
          pexp_loc = e.pexp_loc;
          pexp_attributes = [ (strloc, x) ] }
    | _ -> super.expr self e
  in
  (* type __imp_spec__ = private Spec_xxxx *)
  let forge_spec loc spec =
    let mangled = Spec.to_mangled_string spec in
    Type.mk ~loc
      ~kind: (Ptype_variant [ { pcd_name = {txt=mangled; loc}
                              ; pcd_args = []
                              ; pcd_res = None
                              ; pcd_loc = loc
                              ; pcd_attributes = []
                              } ])
      ~priv: Private
      {txt = "__imp_spec__"; loc}
  in
  let has_typeclass_attr = function
    | {txt="typeclass"}, PStr [] -> true
    | {txt="typeclass"}, _ -> assert false (* CR jfuruse: error *)
    | _ -> false
  in
  let structure self sitems =
    let sitems = flip concat_map sitems & fun sitem ->
      match sitem.pstr_desc with
      | Pstr_modtype mtd when exists has_typeclass_attr mtd.pmtd_attributes ->
          (* module type M = ... [@@typeclass] *)
          (* CR jfuruse: need to remove [@@typeclass] *)
          [ sitem
          ; TypeClass.process_module_type_declaration mtd
          ]
      | Pstr_module mb ->
          (* module M = ... [@@instance Show] *)
          begin match 
            flip filter_map mb.pmb_attributes & function 
              | ({txt="instance"}, 
                  PStr [ { pstr_desc= Pstr_eval ( { pexp_desc= Pexp_construct ({txt; loc}, None) }, []) } ]) -> Some (txt, loc)
              | ({txt="instance"; loc}, _) ->
                  errorf "%a: Syntax error at the instance attribute: it must be [%%%%instance Path]" 
                    Location.format loc
              | _ -> None
          with
          | [] -> [ sitem ]
          | [ (lid, instance_loc) ] ->
              [ sitem;
                TypeClass.instance lid mb
                  ~instance_loc
              ]
          | _ -> assert false
          end
      | _ -> [sitem]
    in
    super.structure self sitems
  in 

  let do_imp_spec loc pld f = match Spec.from_payload pld with
    | `Error err -> Spec.error loc err
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
  { super with expr; structure; structure_item; signature_item }


