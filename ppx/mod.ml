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
let is_implicit_path (p, lid) = 
  let open Path in 
  let open Longident in 
  match p, lid with      
  | Pdot (p, "__imp__", _), Lident "__imp__" -> Some (p, None)
  | Pdot (p, "__imp__", _), Ldot (l, "__imp__") -> Some (p, Some l)
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
(*
val get_candidates : Env.t ->
                          Longident.t ->
                          Types.module_type ->
                          (Longident.t * Path.t * Types.value_description)
                          list
*)
let rec get_candidates env lid mty =
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
  | Sig_value (id, _vdesc) when id.name <> "__imp__" -> 
      let lid = Ldot (lid, Ident.name id) in
      begin try
        let path, vdesc = Env.lookup_value lid env in
        (lid, path, vdesc) :: st
        with
        | e ->
            Format.eprintf "get_candidates: failed to find %a in the current env@." Pprintast.default#longident lid;
            raise e
      end
  | Sig_module (id, _mty, _) -> 
      let lid = Ldot (lid, Ident.name id) in
      let path = 
        try Env.lookup_module ~load:true (*?*) lid env with e ->
          Format.eprintf "get_candidates: failed to find %a in the current env@." Pprintast.default#longident lid;
          raise e
      in
      let moddecl = 
        try Env.find_module path env with e ->
          Format.eprintf "get_candidates: failed to find the module declaration of %a in the current env@." print_path path;
          raise e
      in
      get_candidates env lid moddecl.Types.md_type @ st
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

let is_imp e = 
  let open Parsetree in
  let imps = 
    if e.exp_attributes <> [] then prerr_endline "!";
    flip List.map e.exp_attributes (function 
      | {txt="imp"}, PStr [sitem] ->
        begin match sitem.pstr_desc with
        | Pstr_eval (e, []) ->
            let get_module e = match e.pexp_desc with
              | Pexp_construct ({txt=lid}, None) -> 
prerr_endline "FOUND!";
                  lid
              | _ -> assert false
            in
            begin match e.pexp_desc with
            | Pexp_tuple es -> Some (`Imp1 (List.map get_module es))
            | _ -> Some (`Imp1 [get_module e])
            end
        | _ -> assert false (* illegal imp *)
        end
      | {txt="imp"}, _ -> assert false (* illegal imp *)

      | {txt="imp2"}, PStr [sitem] ->
        begin match sitem.pstr_desc with
        | Pstr_eval (e, []) ->
            let get_module e = match e.pexp_desc with
              | Pexp_construct ({txt=lid}, None) -> 
prerr_endline "FOUND!";
                  lid
              | _ -> assert false
            in
            begin match e.pexp_desc with
            | Pexp_tuple es -> Some (`Imp2 (List.map get_module es))
            | _ -> Some (`Imp2 [get_module e])
            end
        | _ -> assert false (* illegal imp *)
        end
      | {txt="imp2"}, _ -> assert false (* illegal imp *)

      | {txt="imp3"}, PStr [] -> Some `Imp3
      | {txt="imp3"}, _ -> assert false (* illegal imp *)

      | _ -> None)
  in
  match flip List.filter imps & function
    | Some _ -> true
    | None -> false
  with
  | [] -> None
  | [Some x] -> Some x
  | _ -> assert false (* multiple *)
  
let rec get_opens = function
  | Env.Env_empty -> []
  | Env_value (s, _, _)
  | Env_type (s, _, _)
  | Env_extension (s, _, _)
  | Env_module (s, _, _)
  | Env_modtype (s, _, _)
  | Env_class (s, _, _)
  | Env_cltype (s, _, _)
  | Env_functor_arg (s, _) -> get_opens s
  | Env_open (s, path) -> path :: get_opens s

let lids_in_open_path env lids = function
  | None -> 
      flip Ppxx.List.filter_map lids (fun lid ->
        try
          let path = Env.lookup_module ~load:false (*?*) lid env in
          Format.eprintf "CURRENT %a@." print_path path;
          Some path
        with
        | _ -> None)
  | Some (Path.Pident id) when Ident.name id = "Pervasives" && Ident.persistent id  -> 
      (* We assume Pervasives has no instances *)
      []
  | Some open_ ->
      Format.eprintf "open %a@." print_path open_;
      let mdecl = Env.find_module open_ env in (* It should succeed *)
      match Mtype.scrape env mdecl.md_type with
      | Mty_signature sg ->
          let env = Env.open_signature Asttypes.Fresh open_ sg env in
          flip Ppxx.List.filter_map lids (fun lid ->
            try
              let p = Env.lookup_module ~load:false (*?*) lid env in
              Format.eprintf "%a %a@." print_path open_ print_path p;
              Some p
            with
            | _ -> None)
      | _ -> assert false
      
let lids_in_open_paths env lids opens =
  Ppxx.List.concat_map (lids_in_open_path env lids) opens

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let forge3 env loc ty =
    if gen_vars ty <> [] then
      errorf "%a: overloaded value has a generalized type: %a" Location.print_loc loc Printtyp.type_scheme ty;
    let n = match expand_repr_desc env ty with
      | Tconstr (p, _, _) ->
          begin match p with
          | Path.Pdot(p, _ (* __imp__ *), _) ->
              begin match p with
              | Pident id -> Ident.name id
              | Pdot (_, n, _) -> n
              | _ -> assert false
              end
          | _ -> assert false
          end
      | _ -> assert false
    in
    let opens = get_opens & Env.summary env in
    flip List.iter opens (Format.eprintf "open %a@." print_path);
    let paths = List.sort_uniq compare & lids_in_open_paths env [Lident n] (None :: List.map (fun x -> Some x) opens) in
    List.iter (fun p -> Format.eprintf "found %a@." print_path p) paths;
    let cands = List.flatten & flip List.map paths & fun path ->
      match 
        try Some (Env.find_module path env) with _ -> None
      with
      | None -> 
          errorf "%a: no module desc found: %a" Location.print_loc loc print_path path
      | Some mdecl -> get_candidates env (Untypeast.lident_of_path path) mdecl.md_type
    in
    begin match resolve env cands [ty] with
    | [] -> errorf "%a: no instance found for %a" Location.print_loc loc Printtyp.type_expr ty;
    | [[e]] -> e
    | _ -> errorf  "%a: overloaded type has a too ambiguous type: %a" Location.print_loc loc Printtyp.type_expr ty;
    end

  let resolve_arg loc env = function
    (* (l, None, Optional) means not applied *)
    | (l, Some e, Optional as a) when Btype.is_optional l ->
        begin match e.exp_desc with
        | Texp_construct ({Location.txt=Lident "None"}, _, []) ->
            begin match is_option_type env e.exp_type with
            | None -> assert false
            | Some ty ->
                match expand_repr_desc env ty with
                | Tconstr (p, _, _) ->
                    begin match p with
                    | Pident {name = "__imp__"} -> assert false
                    | Pdot (_, "__imp__", _) -> 
                        (l, Some (Forge.Exp.some (forge3 env loc ty)), Optional)
                    | _ -> a
                    end
                | _ -> a
            end
        | _ -> a
        end
    | a -> a

  let app = function
    | ({ exp_desc= Texp_apply (f, args) } as e) ->
        { e with
          exp_desc= Texp_apply (f, List.map (resolve_arg f.exp_loc e.exp_env) args) }
    | e -> e

  let enter_expression e = match is_imp e with
    | None -> app e

    | Some (`Imp1 lids) ->
        let ty = e.exp_type in
        if gen_vars ty <> [] then
          errorf "%a: overloaded value has a generalized type: %a" Location.print_loc e.exp_loc Printtyp.type_scheme ty;
        let env = e.exp_env in
        let cands = List.flatten & flip List.map lids & fun lid ->
          match 
            try Some (Env.lookup_module ~load:true lid env) with _ -> None
          with
          | None ->
              errorf "%a: no module found: %a" Location.print_loc e.exp_loc Pprintast.default#longident lid
          | Some path ->
              match 
                try Some (Env.find_module path env) with _ -> None
              with
              | None -> 
                  errorf "%a: no module desc found: %a" Location.print_loc e.exp_loc print_path path
              | Some mdecl -> get_candidates e.exp_env lid mdecl.md_type
        in
        begin match resolve e.exp_env cands [ty] with
        | [] -> errorf "%a: no instance found for %a" Location.print_loc e.exp_loc Printtyp.type_expr ty;
        | [[e]] -> e
        | _ -> errorf  "%a: overloaded type has a too ambiguous type: %a" Location.print_loc e.exp_loc Printtyp.type_expr ty;
        end

    | Some (`Imp2 lids) ->
        let ty = e.exp_type in
        if gen_vars ty <> [] then
          errorf "%a: overloaded value has a generalized type: %a" Location.print_loc e.exp_loc Printtyp.type_scheme ty;
        let env = e.exp_env in
        let opens = get_opens & Env.summary env in
flip List.iter opens (Format.eprintf "open %a@." print_path);
        let paths = List.sort_uniq compare & lids_in_open_paths env lids (None :: List.map (fun x -> Some x) opens) in
        List.iter (fun p -> Format.eprintf "found %a@." print_path p) paths;
        let cands = List.flatten & flip List.map paths & fun path ->
          match 
            try Some (Env.find_module path env) with _ -> None
          with
          | None -> 
              errorf "%a: no module desc found: %a" Location.print_loc e.exp_loc print_path path
          | Some mdecl -> get_candidates e.exp_env (Untypeast.lident_of_path path) mdecl.md_type
        in
        begin match resolve e.exp_env cands [ty] with
        | [] -> errorf "%a: no instance found for %a" Location.print_loc e.exp_loc Printtyp.type_expr ty;
        | [[e]] -> e
        | _ -> errorf  "%a: overloaded type has a too ambiguous type: %a" Location.print_loc e.exp_loc Printtyp.type_expr ty;
        end

    | Some `Imp3 ->
        forge3 e.exp_env e.exp_loc e.exp_type
end

module Map = TypedtreeMap.MakeMap(MapArg)
