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

let is_dispatch_label l =
  let len = String.length l in
  len >= 3 && String.unsafe_get l 0 = '?' && String.unsafe_get l 1 = '_'

let is_constraint_label l =
  let len = String.length l in
  len >= 2 && String.unsafe_get l 0 = '_'

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
    
(* Oops, there is no good exposed API to compare a module type
   and a packed module type. 
*)

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
  
let rec get_candidates env path mty =
  let sg = 
    match Env.scrape_alias env @@ Mtype.scrape env mty with
    | Mty_signature sg -> sg
    | _ -> assert false
  in
  List.fold_right (fun sitem st -> match sitem with
  | Sig_value (id, _vdesc) -> 
      let lident = Longident.Ldot (Untypeast.lident_of_path path, Ident.name id) in
      let path, vdesc = Env.lookup_value lident env  in
      (path, vdesc) :: st
  | Sig_module (id, _mty, _) -> 
      let lident = Longident.Ldot (Untypeast.lident_of_path path, Ident.name id) in
      let path = Env.lookup_module ~load:true (*?*) lident env  in
      let moddecl = Env.find_module path env in
      get_candidates env path moddecl.Types.md_type @ st
  | _ -> st) sg []

let rec extract_constraint_labels env ty = 
  let ty = Ctype.expand_head env ty in
  match repr_desc ty with
  | Tarrow(l, ty1, ty2, _) when is_constraint_label l ->
      let cs, ty = extract_constraint_labels env ty2 in
      (l,ty1)::cs, ty
  | _ -> [], ty

let gen_vars ty =
  flip List.filter (Ctype.free_variables ty) & fun ty ->
    ty.level = Btype.generic_level

let rec resolve env cands : type_expr list -> expression list list = function
  | [] -> [[]]
  | ty::tys ->
      List.concat & flip List.map cands & fun (path,vdesc) ->
        let ity = Ctype.instance env ty in
        let ivty = Ctype.instance env vdesc.val_type in
        
        let cs, ivty = extract_constraint_labels env ivty in
        Format.eprintf "Got:@.";
        flip List.iter cs (fun (l,ty) ->
          Format.eprintf "  %s:%a ->@." l Printtyp.type_expr ty);
        Format.eprintf "  %a@." Printtyp.type_expr ivty;
(*
        let gvars = gen_vars ty in
*)
        let tys = List.map snd cs @ tys in
        with_snapshot & fun () ->
          try
            Format.eprintf "Checking %a <> %a@."
              Printtyp.type_expr ity
              Printtyp.type_expr ivty;
            ignore & Ctype.unify env ity ivty;
            flip List.map (resolve env cands tys) & fun res ->
              let rec app res cs = match res, cs with
                | res, [] -> res, []
                | r::res, (l,_)::cs ->
                    let res, args = app res cs in
                    res, (l,r)::args
                | _ -> assert false
              in
              let res, args = app res cs in
              Forge.Exp.(app (ident path) args) :: res
          with
          | _ -> []
          
let search_space env =
  try
    let p = Env.lookup_module ~load:true (Longident.Lident "Instance") env in
    Some (p, Env.find_module p env)
  with
  | Not_found -> None
  
module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let resolve_arg env = function
    (* (l, None, Optional) means not applied *)
    | (l, Some e, Optional as a) when is_dispatch_label l -> 
        begin match e.exp_desc with
        | Texp_construct ({txt=Longident.Lident "None"}, _, []) ->
            begin match is_option_type env e.exp_type with
            | None -> assert false
            | Some ty ->
                let e = 
                  match search_space env with
                  | None -> assert false
                  | Some (path, mdecl) ->
                      let cands = get_candidates env path mdecl.md_type in
                      match resolve env cands [ty] with
                      | [] -> failwith "overload resolution failed: no match" 
                      | [[e]] -> Forge.Exp.some e
                      | _ -> failwith "overload resolution failed: too ambiguous" 
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
    | e -> e
end

module Map = TypedtreeMap.MakeMap(MapArg)
