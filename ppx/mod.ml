open Types
open Typedtree
open Ppxx

open Longident

let errorf fmt =
  let open Format in
  let ksprintf f fmt =
    let buf = Buffer.create 100 in
    let ppf = formatter_of_buffer buf in
    kfprintf (fun ppf -> pp_print_flush ppf (); f (Buffer.contents buf)) ppf fmt
  in
  ksprintf (fun s -> prerr_endline s; exit 1) fmt

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

(* XXX.IMP.t *)
let is_implicit_path p = 
  let open Path in 
  match p with      
  | Pdot (Pdot (p, "IMP", _), "t", _) -> Some p
  | _ -> None

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
  
(* Recursively visit the module and get values defined in it *)
let rec get_candidates env path mty =
  let sg = 
    try
      match Env.scrape_alias env @@ Mtype.scrape env mty with
      | Mty_signature sg -> sg
      | _ -> assert false
    with
    | e ->
        prerr_endline "get_candidates: scraping failed";
        raise e
  in
  flip2 List.fold_right sg [] & fun sitem st -> match sitem with
  | Sig_value (id, _vdesc) -> 
      let p = Path.Pdot (path, Ident.name id, Ident.stamp id) in
      begin try
        let vdesc = Env.find_value p env in
        (p, vdesc) :: st
        with
        | e ->
            Format.eprintf "get_candidates: failed to find %a in the current env@." print_path p;
            raise e
      end
  | Sig_module (id, _mty, _) -> 
      let p = Path.Pdot (path, Ident.name id, Ident.stamp id) in
      let moddecl = 
        try Env.find_module p env with e ->
          Format.eprintf "get_candidates: failed to find %a in the current env@." print_path p;
          raise e
      in
      get_candidates env p moddecl.Types.md_type @ st
  | _ -> st

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
      List.concat & flip List.map cands & fun (lid,path,vdesc) ->
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
              Forge.Exp.(app (ident lid path) args) :: res
          with
          | _ -> []
          
let search_space env =
  try
    let lid = Lident "Instance" in
    let p = Env.lookup_module ~load:true lid env in
    Some (lid, p, Env.find_module p env)
  with
  | Not_found -> None
  
module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let resolve_arg f env = function
    (* (l, None, Optional) means not applied *)
    | (l, Some e, Optional as a) when is_dispatch_label l -> 
        begin match e.exp_desc with
        | Texp_construct ({Location.txt=Lident "None"}, _, []) ->
            begin match is_option_type env e.exp_type with
            | None -> assert false
            | Some ty ->
                let e = 
                  match search_space env with
                  | None ->
                      errorf "%a: no instance search space Instance found" Location.print_loc f.exp_loc
                  | Some (lid, _path, mdecl) ->
                      let cands = get_candidates env lid mdecl.md_type in
                      if gen_vars ty <> [] then
                        errorf "%a: overloaded value has a generalized type: %a" Location.print_loc f.exp_loc Printtyp.type_scheme ty;
                      match resolve env cands [ty] with
                      | [] -> errorf "%a: no instance found for %a" Location.print_loc f.exp_loc Printtyp.type_expr ty;
                      | [[e]] -> Forge.Exp.some e
                      | _ -> errorf  "%a: overloaded type has a too ambiguous type: %a" Location.print_loc f.exp_loc Printtyp.type_expr ty;
                in
                (l, Some e, Optional)
            end
        | Texp_construct ({Location.txt=Lident "Some"}, _, [_]) -> a
        | _ -> 
            (* This is problematic: https://bitbucket.org/camlspotter/ppx_typeclass/issue/1/prevent-indirect-application-of-none-to
            *)
            a
        end
    | a -> a

  let enter_expression e =
    match e.exp_desc with
    | Texp_construct ({Location.txt=Lident "None"}, _, []) ->
        begin match is_option_type e.exp_type with
        | None -> e
        | Some ty ->
            match expand_repr_desc e.exp_env ty with
            | Tconstr (p, [ty], _) ->
                begin match is_implicit_path p with
                | None -> e
                | Some p' ->
                    match 
                      try
                        Some (Env.find_module p' e.exp_env)
                      with
                      | _ -> None
                    with
                    | None ->
                        errorf "%a: no instance search space found: %a" print_path p'
                    | Some mdecl ->
                        let cands = get_candidates env path mdecl.md_type in
                        if gen_vars ty <> [] then
                          errorf "%a: overloaded value has a generalized type: %a" Location.print_loc f.exp_loc Printtyp.type_scheme ty;
                        match resolve env cands [ty] with
                        | [] -> errorf "%a: no instance found for %a" Location.print_loc f.exp_loc Printtyp.type_expr ty;
                        | [[e]] -> Forge.Exp.some e
                        | _ -> errorf  "%a: overloaded type has a too ambiguous type: %a" Location.print_loc f.exp_loc Printtyp.type_expr ty;
                end
            | _ -> e
        end
    | _ -> e

end

module Map = TypedtreeMap.MakeMap(MapArg)
