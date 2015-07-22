(*

  Pre-preprocessing for syntax sugars for [%imp] and [%%imp_policy].

*)

open Ast_helper
open Ppxx (* extends some of Ast_helper *)

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

  (* Fake class 

     From 

        module type Show = sig
          type a 
          val show : a -> string
        end

     it builds

        module Show : struct
          type 'a t
          let show (type a) : ?_imp:a t -> a -> string = fun ?_imp -> assert false
        end
  *)

  (* ex. obtain 'a' from the definition of module type Show *)
  let parameters sg = sort compare & concat_map (fun si ->
    match si.psig_desc with
    | Psig_type tds ->
        filter_map (fun td ->
          match td with
          | { ptype_name = {txt}; ptype_params = []; ptype_cstrs = []; ptype_kind = Ptype_abstract; ptype_manifest = None; } -> Some txt
          | _ -> None) tds
    | _ -> []) sg

  (* ex. obtain show : a -> string from the definition of module type Show *)
  let values sg = filter_map (fun si ->
    match si.psig_desc with
      | Psig_value vdesc -> Some (vdesc.pval_name.txt, vdesc.pval_type)
      | _ -> None) sg

(*
  (* type ('a, 'b) _class *)
  let fake_typ ps n =
    let tvars = map (Typ.var ?loc:None) ps in (* CR jfuruse: loc *)
    Type.mk ?loc:None
      ~params: (map (fun tv -> (tv, Invariant)) tvars)
      ~kind: Ptype_abstract
      (at n)
*)

  let add_newtypes = fold_right (fun s -> Exp.newtype ?loc:None s)
(*

  (* let n (type a) .. : ?_imp:(a,..) _class -> ty = fun ?_imp -> assert false *)
  let fake_method ps n ty =
    let pat = Pat.var ?loc:None n in
    let ty_class = 
      Typ.constr (at ?loc:None & Lident "_class") 
      & map (fun p -> Typ.constr (at (Lident p)) []) ps
    in
    let exp = 
      add_newtypes ps 
      & Exp.constraint_ [%expr fun ?_imp -> assert false]
          (Typ.arrow "?_imp" (Typ.option ty_class) ty)
    in
    Str.value ?loc:None Nonrecursive [ Vb.mk ?loc:None pat exp ]
*)

  (* [%%imp_policy opened ShowInstance] *)
  let policy name = 
    let opened = 
      [%expr opened [%e Exp.construct (at ?loc:None (Lident (name ^ "Instance"))) None ]] 
    in
    Str.extension ?loc:None (at "imp_policy", PStr [(Str.eval opened)])

(*
  (* module Show = struct
       type 'a _class
       let show (type a) : ?_imp:a _class -> a -> string = fun ?_imp -> assert false
     end 
  *)
  let fake_class cname ps vs =
    let tds = 
      [ Str.type_ ?loc:None [ fake_typ ps "_class" ];
        Str.type_ ?loc:None [ fake_typ ps "_module" ] ]
    in
    let methods = map (fun (n,ty) -> fake_method ps n ty) vs in
    Str.module_ ?loc:None & 
      Mb.mk ?loc:None 
        (at ?loc:None cname) 
        (Mod.structure ?loc:None (tds @ methods @ [ policy cname ]))
    
  let fake_class mtd =
    let cname = mtd.pmtd_name.txt in
    match mtd.pmtd_type with 
    | Some { pmty_desc = Pmty_signature sg } -> 
        let ps = parameters sg in
        let vs = values sg in
        begin match ps with
        | [] -> assert false (* error. no parameters *)
        | _ -> fake_class cname ps vs
        end
    | Some _ -> assert false (* error *)
    | None -> assert false (* error *)
*)
    

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
    let paramed_s = Typ.(constr (at ?loc:None (Lident "_module")) (map (fun ty -> constr ?loc:None (at ?loc:None (Lident ty)) []) tys)) 
    in
    [%stri let [%p Pat.var ?loc:None n] =
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
               & Str.type_ [gen_ty_module name ps]
                 :: Str.type_ [gen_ty_class ps]
                 :: policy name
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

     module ShowInstance = struct
       let int : int Show._module = (module ShowInt)
     end
  *)
  let instance lid mb =  
    let cname = match lid with
      | Lident cname -> cname
      | Ldot (_, cname) -> cname
      | _ -> assert false (* CR jfuruse: error *)
    in
    let iname = mb.pmb_name.txt in
    let oname = cname ^ "Instance" in
    let str =match mb.pmb_expr.pmod_desc with
      | Pmod_structure str -> str
      | _ -> assert false (* CR jfuruse: error handling *)
    in
    let ps = parameters str in
    Str.module_ ?loc:None & Mb.mk ?loc:None (at ?loc:None oname)
      (Mod.structure ?loc:None [
         Str.value ?loc:None Nonrecursive 
           [ Vb.mk ?loc:None 
               (Pat.var ?loc:None "dict")
               (Exp.constraint_ ?loc:None 
                 (Exp.pack ?loc:None (Mod.ident ?loc:None (at ?loc:None (Lident iname))))
                 (Typ.constr ?loc:None 
                   (at ?loc:None (Ldot (lid, "_module")))
                   (map (fun (p,_) -> Typ.constr ?loc:None (at ?loc:None (Ldot (Lident iname, p))) []) ps)))
           ]])
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
  let structure self sitems =
    let sitems = concat_map (fun sitem ->
      match sitem.pstr_desc with
      | Pstr_modtype mtd when List.exists has_typeclass_attr mtd.pmtd_attributes ->
          (* CR jfuruse: need to remove [@@typeclass] *)
          [ sitem
          ; TypeClass.process_module_type_declaration mtd
          ]
      | Pstr_module mb ->
          begin match 
            filter_map (function 
                    | ({txt="instance"}, 
                        PStr [ { pstr_desc= Pstr_eval ( { pexp_desc= Pexp_construct ({txt}, None) }, []) } ]) -> Some txt
                    | ({txt="instance"}, _) -> assert false (* CR jfuruse: error *)
                    | _ -> None) mb.pmb_attributes
          with
          | [] -> [ sitem ]
          | [lid] -> [sitem; TypeClass.instance lid mb]
          | _ -> assert false
          end
      | _ -> [sitem]) sitems
    in
    super.structure self sitems
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
  { super with expr; structure; structure_item; signature_item }


