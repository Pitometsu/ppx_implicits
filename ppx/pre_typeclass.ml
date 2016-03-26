(*

  Pre-preprocessing for syntax sugars for 
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

  (* module type Show = sig 
     type a 
     type b
     val show : a -> b -> string 
     end
     => [a; b]
  *)
  let parameters sg = sort compare & concat_map (fun si ->
    match si.psig_desc with
    | Psig_type tds ->
        flip filter_map tds & fun td ->
          begin match td with
          | { ptype_name = {txt}; ptype_params = []; ptype_cstrs = []; ptype_kind = Ptype_abstract; ptype_manifest = None; } -> Some txt
          | _ -> None
          end
    | _ -> []) sg

  (* module type Show = sig 
       type a 
       type b
       val show : a -> b -> string 
     end
     => ["show", a -> b -> string]
  *)
  let values = filter_map & fun si ->
    match si.psig_desc with
      | Psig_value vdesc -> Some (vdesc.pval_name.txt, vdesc.pval_type)
      | _ -> None

  let add_newtypes = fold_right (fun s -> Exp.newtype ?loc:None s)

  let link = [%stri type __class__ ]

  (* type ('a, 'b) _module = (module Show with type a = 'a and b = 'b) *)
  let gen_ty_module name ps =
    let tvars = map (Typ.var ?loc:None) ps in (* CR jfuruse: loc *)
    Type.mk ?loc:None
      ~params: (map (fun tv -> (tv, Invariant)) tvars)
      ~manifest: (Typ.package ?loc:None 
                   (at (Lident name)) 
                   (map2 (fun p tv -> (at (Lident p), tv)) ps tvars))
      (at "_module") (* CR jfuruse: can have a ghost loc *)

  (* type ('a, 'b) _class = (('a, 'b) _module, [%imp_spec has_type __class__]) Ppx_implicits.t *)
  let gen_ty_class ps =
    let tvars = map (Typ.var ?loc:None) ps in (* CR jfuruse: loc *)
    Type.mk ?loc:None
      ~params: (map (fun tv -> (tv, Invariant)) tvars)
      ~manifest: [%type: ([%t Typ.constr (at (Lident "_module")) tvars], [%imp_spec has_type __class__]) Ppx_implicits.t]
      (at "_class") (* CR jfuruse: can have a ghost loc *)
  ;;

  (* let show (type a) ?_d:(_d : a _class option) =
    let module M = (val (Ppx_implicits.(get (from_Some _d)))) in
    M.show
  *)

  let method_ tys (n,_ty) = 
    let paramed_s = 
      let open Typ in
      constr (at ?loc:None & Lident "_class") 
      & map (fun ty -> constr ?loc:None (at ?loc:None & Lident ty) []) tys
    in
    [%stri let [%p Pat.var' ?loc:None n] =
        [%e add_newtypes tys 
            [%expr fun ?_d:(_d : [%t paramed_s] option) -> 
                     let module M = (val (Ppx_implicits.(get (from_Some _d)))) in
                     [%e Exp.(ident ?loc:None (at ?loc:None (Ldot (Lident "M", n)))) ] ] ] ]

  let process_module_type_declaration mtd =
    let name = mtd.pmtd_name.txt in
    match mtd.pmtd_type with 
    | Some { pmty_desc = Pmty_signature sg } -> 
        let ps = parameters sg in
        let vs = values sg in
        begin match ps with
        | [] ->
            errorf "%a: sig .. end [@@@@typeclass] requires at least one parameter type declaration like type a" Location.format mtd.pmtd_loc
        | _ -> 
            Str.module_ ?loc:None & Mb.mk ?loc:None (at ?loc:None name)
              (Mod.structure ?loc:None 
               & [%stri [@@@warning "-16"]]
                 :: Str.type_ [gen_ty_module name ps]
                 :: link
                 :: Str.type_ [gen_ty_class ps]
                 :: map (method_ ps) vs
              )
        end
    | _ -> errorf "%a: a signature (sig .. end) is required for [@@@@typeclass]" Location.format mtd.pmtd_loc

  let parameters str =
    sort (fun (x,_) (y,_) -> compare x y) & concat_map (fun si ->
      match si.pstr_desc with
      | Pstr_type tds ->
          filter_map (fun td ->
            match td with
            | { ptype_name = {txt}; ptype_params = []; ptype_cstrs = []; ptype_kind = Ptype_abstract; ptype_manifest = Some ty; } -> Some (txt, ty)
            | _ -> None) tds
      | _ -> []) str

  (* ((module <m>) : (<m>.a, <m>.b) <o>._module) *)
  let dict_module m o ps =
    Exp.constraint_ ?loc:None 
      (Exp.pack ?loc:None & Mod.ident' ?loc:None & Lident m)
      (Typ.constr ?loc:None (at ?loc:None & Ldot (Lident o, "_module"))
       & map (fun (p,_) -> Typ.constr ?loc:None (at ?loc:None & Ldot (Lident m, p)) []) ps)


  module Dict = struct
      
    (* [param d tvs m] = (d : (<tvs> <m>._module, [%imp_spec has_type <m>.__class__]) Ppx_implicits.t option) *)
    let param d tvs m =
      Pat.constraint_ (Pat.var' d)
      & Typ.(let a = constr (at & Ldot (m, "_module"))
               (map (fun tv -> constr (at & Lident tv) []) tvs)
             in
             let b = Specconv.to_core_type Location.none & Spec.(Or [Has_type (Typ.(constr (at & Ldot (m, "__class__")) []), None)]) in
             [%type: ([%t a], [%t b]) Ppx_implicits.t option])
        
    (* let dict (type a) ?d:(d : (a Numdef.Num._module, [%imp_spec has_type Numdef.Num.__class__]) Ppx_implicits.t option) = *)
  
    (* val (Ppx_implicits.(get (from_Some <d>))) *)
    let functor_arg d = Mod.unpack [%expr Ppx_implicits.(get (from_Some [%e d])) ]
  
    (* ((module <n> : (<n>.a, <n>.b) <o>._module)
       
       or
       
       let module M = <n>((val (Ppx_implicits.(get (from_Some d)))))..(..) in
       ((module M) : (M.a, M.b) <o>._module)
    *)
    let z n ds o ps = match ds with
      | [] -> dict_module n o ps
      | _ -> 
          Exp.letmodule (at "M") (fold_left Mod.apply (Mod.ident (at & Lident n)) & map functor_arg ds)
          & dict_module "M" o ps

    (* let dict (type a) (type b).. ?d1:(d1 : (a m1._module, [%imp_spec has_type m1.__class__]) Ppx_implicits.t option) ?d2:(d2 : (b m2._module, [...]) =
       let module M = <n>((val (Ppx_implicits.(get (from_Some d1))))).. in
       ((module M) : (M.ps1, M.psn) <o>._module)
    *)
    let dict f (* functor *)
             p_mty (* module defines the module type *)
             ps (* parameters of type class *)
             ks (* constraints *) =
      let ds = mapi (fun i _ -> "d" ^ string_of_int i) ks in
      let pats = map2 (fun d (tvs, m) -> param d tvs m) ds ks in
      let tvs = concat & map (fun (tvs, _) -> tvs) ks in
      let e = z f (map (fun i -> Exp.ident (at & Lident i)) ds) p_mty ps in
      let e = fold_left2 (fun e d p -> Exp.fun_ ("?"^d) None p e) e ds pats in
      [%stri let dict = [%e fold_left (flip Exp.newtype) e tvs]]
  end
                                
  (* 
     module ShowInt = struct
       type a = int
       let show  = string_of_int
     end [@@instance Show] 

     =>

     module ShowIntInstance = struct
       let dict : ShowInt.a Show.s = (module ShowInt)
       type __imp_instance_of__ = Show.__class__
     end
  *)
  let instance lid mb ~instance_loc =  
    let cname = match lid with (* Show *)
      | Lident cname -> cname
      | Ldot (_, cname) -> cname
      | _ ->
          errorf "%a: %a is invalid for type class"
            Location.format instance_loc
            Longident.format lid
    in
    let iname = mb.pmb_name.txt in (* ShowInt *)
    let oname = iname ^ "Instance" in (* ShowIntInstance *)
    let str, ks =
      let rec get_str me = match me.pmod_desc with
        | Pmod_structure str -> str, []
        | Pmod_constraint (me, _) -> get_str me
        | Pmod_functor (_, Some { pmty_attributes = attrs }, me) ->
            begin match
                filter_map (function
                  | ({txt="typeclass"}, PStr [{pstr_desc=Pstr_eval (e,_)}]) -> Some e
                  | ({txt="typeclass"; loc}, _) ->
                      errorf "%a: Invalid syntax of @@typeclass for functor argument.  It must be [@@typeclass <params> <modname>]"
                        Location.format loc
                  | _ -> None) attrs
              with
              | [] -> get_str me
              | [e] ->
                  let k = match e.pexp_desc with
                    | Pexp_apply (tvs, ["", mp]) ->
                        let tvs =
                          let get_var tv = match tv.pexp_desc with
                            | Pexp_ident {txt=Lident s} -> s
                            | _ ->
                                errorf "%a: Invalid syntax of @@typeclass parameter"
                                  Location.format tv.pexp_loc
                          in
                          match tvs.pexp_desc with
                          | Pexp_tuple es -> map get_var es
                          | _ -> [get_var tvs]
                        in
                        let lid = match mp.pexp_desc with
                          | Pexp_construct ({txt=lid}, None) -> lid
                          | _ ->
                              errorf "%a: Invalid syntax of @@typeclass module"
                                Location.format mp.pexp_loc
                        in
                        tvs, lid
                    | _ -> 
                        errorf "%a: Invalid syntax of @@typeclass for functor argument.  It must be [@@typeclass <params> <modname>]"
                          Location.format e.pexp_loc
                  in
                  let str, ks = get_str me in
                  str, k::ks
              | _ ->
                  errorf "%a: multiple @@typeclass attributes found"
                    Location.format me.pmod_loc
            end
        | Pmod_functor (_, _, me) -> get_str me
        | _ ->
            errorf "%a: Invalid module for @@@@instance"
              Location.format me.pmod_loc
      in
      get_str mb.pmb_expr
    in
    let ps = parameters str in
    with_gloc mb.pmb_loc & fun () ->
        Str.module_ & Mb.mk (at ~loc:mb.pmb_name.loc oname)
        & Mod.structure ~loc:mb.pmb_expr.pmod_loc
          [ Dict.dict iname cname ps ks
          ; with_gloc instance_loc & fun () ->
            Str.type_ [ Type.mk ~manifest:(Typ.constr (at & Ldot (Lident cname, "__class__")) []) (at "__imp_instance_of__") ]
          ]
end

(* module type S = sig .. end [@@typeclass] *)
(* module M = struct .. end [@@instance C] *)
let extend super =
  let has_typeclass_attr = function
    | {txt="typeclass"}, PStr [] -> true
    | {txt="typeclass"; loc}, _ ->
        errorf "%a: [@@@@typeclass] must not take payload"
          Location.format loc
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
              [ sitem
              ; TypeClass.instance lid mb ~instance_loc
              ]
          | _ -> 
              errorf "%a: multiple [@@@@instance] found"
                Location.format mb.pmb_loc
          end
      | _ -> [sitem]
    in
    super.structure self sitems
  in 
  { super with structure }
