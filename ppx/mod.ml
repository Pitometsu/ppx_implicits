open Types
open Typedtree
open Ppxx

let repr_desc ty = (Ctype.repr ty).desc
let expand_repr_desc env ty = (Ctype.repr & Ctype.expand_head env ty).desc

let print_ident ppf id = Format.fprintf ppf "%s/%d" id.Ident.name id.Ident.stamp

let rec print_path ppf = function
  | Path.Pident id -> print_ident ppf id
  | Path.Pdot (p, name, n) -> Format.fprintf ppf "%a.%s__%d" print_path p name n
  | Path.Papply (p1, p2) -> Format.fprintf ppf "%a(%a)" print_path p1 print_path p2

let get_name = function
  | Path.Pident id -> Ident.name id
  | Path.Pdot (_, name, _) -> name
  | Path.Papply _ -> assert false

let protect f = try `Ok (f ()) with e -> `Error e
let unprotect = function
  | `Ok v -> v
  | `Error e -> raise e

let with_snapshot f =
  let snapshot = Btype.snapshot () in
  let res = protect f in
  Btype.backtrack snapshot;
  unprotect res

let with_current_level x f =
  let open Ctype in
  begin_def (); (* save it *)
  init_def x;
  let res = protect f in
  end_def ();
  unprotect res

let force_generalize ty =
  with_current_level Btype.lowest_level & fun () ->
    Ctype.generalize ty (* it is destructive! *)
    
(* 
   ty : context type
   vdesc : packed type of class instance

   vdesc.val_type -> instantiate -> ity
   if ity has _d:aaa -> ... -> ity' 
   unify ty and ity'
   check ty is unchhanged
   return aaa to solve sub-problems
*)
let test env ty vdesc =
  with_snapshot & fun () ->
    force_generalize ty;
    let b = Ctype.moregeneral env true vdesc.val_type ty in
    Format.eprintf "Check against %a  <>  %a@." 
      Printtyp.type_scheme vdesc.val_type
      Printtyp.type_scheme ty;
    b
    


(* Oops, there is no good exposed API to compare a module type
   and a packed module type. 
*)

let is_dispatch_label l =
  let len = String.length l in
  len >= 3 && String.unsafe_get l 0 = '?' && String.unsafe_get l 1 = '_'

let is_constr env ty = match expand_repr_desc env ty with
  | Tconstr (p, tys, _) -> Some (p, tys)
  | _ -> None

let is_option_type env ty = match is_constr env ty with
  | Some (po, [ty]) when po = Predef.path_option -> Some ty
  | _ -> None

let check_entrypoint env vdesc =
  match expand_repr_desc env vdesc.val_type with
  | Tarrow (l, ty1, ty2, _) when is_dispatch_label l ->
      begin match is_option_type env ty1 with
      | None -> None
      | Some ty ->
          match expand_repr_desc env ty with
          | Tpackage _ -> Some (l, ty, ty2)
          | _ -> assert false
      end
  | _ -> None
  
(*
let is_type_class_modtype ty =
  match repr_desc ty with
  | Tpackage _ -> true (* it must be (module X) *)
  | _ -> false

let is_tvar ty = 
  let ty = Ctype.repr ty in
  match ty.desc with
  | Tvar _ -> Some ty
  | _ -> None

let check_entrypoint env vdesc =
  match repr_desc vdesc.val_type with
  | Tarrow (l, ty1, ty2, _) when Btype.is_optional l ->
      begin match Option.map repr_desc & is_option_type env ty1 with
      | Some ty ->
          begin match is_constr env ty with
          | Some (p, [ty]) ->
              begin match is_tvar ty with
              | Some ty -> Some (p, ty)
              | _ -> None
              end
          | _ -> None
          end
      | _ -> None
      end
  | _ -> None
  (* CR jfuruse: need to check if the type is aliased to 'a t  *)
*)

let search_space env =
  try
    let p = Env.lookup_module ~load:true (Longident.Lident "Instance") env in
    Some (p, Env.find_module p env)
  with
  | Not_found -> None

(*
val find_candidates : Env.t ->
                           Types.type_expr ->
                           Path.t ->
                           Types.module_type ->
                           (Path.t * Types.value_description) list
*)
let rec find_candidates env ty path mty =
  (* Format.eprintf "Find_candidates %a@." print_path path; *)

  let sg = 
    match Env.scrape_alias env @@ Mtype.scrape env mty with
    | Mty_signature sg -> sg
    | _ -> assert false
  in
  List.fold_right (fun sitem st -> match sitem with
  | Sig_value (id, _vdesc) -> 
      let lident = Longident.Ldot (Untypeast.lident_of_path path, Ident.name id) in
      let path, vdesc = Env.lookup_value lident env  in
      if test env ty vdesc then (path, vdesc) :: st else st
  | Sig_module (id, _mty, _) -> 
      let lident = Longident.Ldot (Untypeast.lident_of_path path, Ident.name id) in
      let path = Env.lookup_module ~load:true (*?*) lident env  in
      let moddecl = Env.find_module path env in
      find_candidates env ty path moddecl.Types.md_type @ st
  | _ -> st) sg []
  
let loc txt = { Location.txt; loc = Location.none }

let resolve_entrypoint exp path vdesc = 

  let env = exp.exp_env in
  let name = get_name path in

  match check_entrypoint env vdesc with
  | None -> failwith "strange type for an overloaded entrypoint"
  | Some (_string, packed_mty, _ty) ->

  match search_space env with
  | None -> assert false
  | Some (path, mdecl) ->

  match 
    find_candidates env packed_mty path mdecl.md_type
  with
  | [] -> failwith "overload resolution failed: no match" 
  | [path, vdesc] -> 
      Format.eprintf "RESOLVED: %a@." print_path path;
      let ity = Ctype.instance env vdesc.val_type in
      Format.eprintf "RESOLVED against %a  <>  %a@." 
        Printtyp.type_expr packed_mty
        Printtyp.type_expr ity;
      Ctype.unify env packed_mty ity; (* should succeed *)
      (* plus => let plus = let module X = (val Instance.int) in X.plus in plus 
*)
      let open Forge in
      (* (val Instance.int) *)
      let mexpr = Forge.Mod.unpack (Exp.ident path) in
      let expr1 = 
        let tmp_id = Ident.create "X" in
        let path = Path.Pdot (Path.Pident tmp_id, name, 0) in
        Exp.letmodule tmp_id mexpr & Exp.ident path
      in
      let tmp_id = Ident.create name in
      let tmp_id_exp = Exp.ident (Pident tmp_id) in
      let vb = { vb_pat = Pat.var tmp_id;
                 vb_expr = expr1;
                 vb_attributes = [];
                 vb_loc = Location.none }
      in
      Exp.let_ [vb] tmp_id_exp
  | _ -> failwith "overload resolution failed: too ambiguous" 

let resolve_dispatch env ty = 

  match search_space env with
  | None -> assert false
  | Some (path, mdecl) ->

  match 
    find_candidates env ty path mdecl.md_type
  with
  | [] -> failwith "overload resolution failed: no match" 
  | [path, vdesc] -> 
      Format.eprintf "RESOLVED: %a@." print_path path;
      let ity = Ctype.instance env vdesc.val_type in
      Format.eprintf "RESOLVED against %a  <>  %a@." 
        Printtyp.type_expr ty
        Printtyp.type_expr ity;
      Ctype.unify env ty ity; (* should succeed *)
      { exp_desc = Texp_ident (path, loc (Untypeast.lident_of_path path), 
                               vdesc);
        exp_loc = Location.none;
        exp_extra = [];
        exp_type = ty;
        exp_env = env;
        exp_attributes = []
      }
  | _ -> failwith "overload resolution failed: too ambiguous" 

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

(*
  let is_entrypoint e = match e.exp_desc with
    | Texp_ident (path, lidloc, vdesc) ->
        begin match vdesc.val_kind with
        | Val_prim { Primitive.prim_name = "%OVERLOADED" } ->
            Some (e, lidloc, path, vdesc)
        | _ -> None
        end
    | _ -> None
*)
    
  let resolve_arg env = function
    (* (l, None, Optional) means not applied *)
    | (l, Some e, Optional as a) when is_dispatch_label l -> 
        begin match e.exp_desc with
        | Texp_construct ({txt=Longident.Lident "None"}, _, []) ->
            begin match is_option_type env e.exp_type with
            | None -> assert false
            | Some ty ->
                let e = 
                  let some_lid = Longident.Lident "Some" in
                  let cdesc = Env.lookup_constructor some_lid env in
                  
                  { e with exp_desc = 
                      Texp_construct(loc some_lid, cdesc, [resolve_dispatch env ty]) }
                in
                (l, Some e, Optional)
            end
        | _ -> a
        end
    | a -> a

  let enter_expression = function
    | ({ exp_desc= Texp_apply (f, args) } as e) ->
        { e with exp_desc= Texp_apply (f, 
                                       List.map (resolve_arg e.exp_env) args) }
(* Optimization. Think it later.
    | ({ exp_desc= Texp_ident (path, lidloc, vdesc) } as e)-> 
        begin match vdesc.val_kind with
        | Val_prim { Primitive.prim_name = "%OVERLOADED" } ->
            (* additional check required:
               * It has one (or more) optional argument(s) of type
                 (module X with type a = ..) Dispatch.t
             *)
            resolve_entrypoint e lidloc path vdesc
        | _ -> e
        end
*)
    | e -> e
end

module Map = TypedtreeMap.MakeMap(MapArg)
