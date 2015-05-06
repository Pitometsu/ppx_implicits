open Types
open Typedtree
open Ppxx
open Longident (* has flatten *)
open List (* has flatten *)

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

(* XXX.IMP.t *)
let is_implicit_path (p, lid) = 
  let open Path in 
  let open Longident in 
  match p, lid with      
  | Pdot (p, "__imp__", _), Lident "__imp__" -> Some (p, None)
  | Pdot (p, "__imp__", _), Ldot (l, "__imp__") -> Some (p, Some l)
  | _ -> None

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

(* Oops, there is no good exposed API to compare a module type
   and a packed module type. 
*)

let is_constr env ty = match expand_repr_desc env ty with
  | Tconstr (p, tys, _) -> Some (p, tys)
  | _ -> None

let is_option_type env ty = match is_constr env ty with
  | Some (po, [ty]) when po = Predef.path_option -> Some ty
  | _ -> None

let rec extract_constraint_labels env ty = 
  let ty = Ctype.expand_head env ty in
  match repr_desc ty with
  | Tarrow(l, ty1, ty2, _) when is_constraint_label l ->
      let cs, ty = extract_constraint_labels env ty2 in
      (l,ty1)::cs, ty
  | _ -> [], ty

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
  flip2 fold_right sg [] & fun sitem st -> match sitem with
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
          Format.eprintf "get_candidates: failed to find the module declaration of %a in the current env@." Path.format path;
          raise e
      in
      get_candidates env lid moddecl.Types.md_type @ st
  | _ -> st

let gen_vars ty =
  flip filter (Ctype.free_variables ty) & fun ty ->
    ty.level = Btype.generic_level

let rec resolve env cands : ((Path.t * type_expr) list * type_expr) list -> expression list list = function
  | [] -> [[]]
  | (trace,ty)::tr_tys ->
      concat & flip map cands & fun (lid,path,vdesc) ->
         match
           try Some (assoc path trace) with _ -> None
         with
         | Some ty' when not & Tysize.(lt (size ty) (size ty' )) ->
             (* recursive call and the type size is not strictly decreasing *)
             Format.eprintf "ADM failure: %a : %a  =>  %a@." 
               Path.format path Printtyp.type_expr ty' Printtyp.type_expr ty;
             []
         | _ ->
             let trace' = (path, ty) :: trace in (* CR jfuruse: Older binding is no longer useful. Replace instead of add? *)

             let ity = Ctype.instance env ty in
             let ivty = Ctype.instance env vdesc.val_type in
        
             let cs, ivty = extract_constraint_labels env ivty in
             Format.eprintf "Got:@.";
             flip iter cs (fun (l,ty) ->
               Format.eprintf "  %s:%a ->@." l Printtyp.type_expr ty);
             Format.eprintf "  %a@." Printtyp.type_expr ivty;
             let tr_tys = map (fun (_,ty) -> (trace',ty)) cs @ tr_tys in
             with_snapshot & fun () ->
               try
                 Format.eprintf "Checking %a <> %a@."
                 Printtyp.type_expr ity
                 Printtyp.type_expr ivty;
                 ignore & Ctype.unify env ity ivty;
                 flip map (resolve env cands tr_tys) & fun res ->
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
          
let is_imp e = 
  let open Parsetree in
  let get_modules sitem = match sitem.pstr_desc with
    | Pstr_eval (e, []) ->
        let get_module e = match e.pexp_desc with
          | Pexp_construct ({txt=lid}, None) -> lid
          | _ -> assert false
        in
        begin match e.pexp_desc with
        | Pexp_tuple es -> Some (map get_module es)
        | _ -> Some [get_module e]
        end
    | _ -> assert false (* illegal imp *)
  in
  let imps = 
    if e.exp_attributes <> [] then prerr_endline "!";
    flip map e.exp_attributes (function 
      | {txt="imp"}, PStr [sitem] -> 
          Option.map (fun x -> `Imp1 x) & get_modules sitem
      | {txt="imp"}, _ -> assert false (* illegal imp *)

      | {txt="imp2"}, PStr [sitem] ->
          Option.map (fun x -> `Imp2 x) & get_modules sitem
      | {txt="imp2"}, _ -> assert false (* illegal imp *)

      | {txt="imp3"}, PStr [] -> Some `Imp3
      | {txt="imp3"}, _ -> assert false (* illegal imp *)

      | _ -> None)
  in
  match flip filter imps & function
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
      flip filter_map lids (fun lid ->
        try
          let path = Env.lookup_module ~load:false (*?*) lid env in
          Format.eprintf "CURRENT %a@." Path.format path;
          Some path
        with
        | _ -> None)
  | Some (Path.Pident id) when Ident.name id = "Pervasives" && Ident.persistent id  -> 
      (* We assume Pervasives has no instances *)
      []
  | Some open_ ->
      Format.eprintf "open %a@." Path.format open_;
      let mdecl = Env.find_module open_ env in (* It should succeed *)
      match Mtype.scrape env mdecl.md_type with
      | Mty_signature sg ->
          let env = Env.open_signature Asttypes.Fresh open_ sg env in
          flip filter_map lids (fun lid ->
            try
              let p = Env.lookup_module ~load:false (*?*) lid env in
              Format.eprintf "%a %a@." Path.format open_ Path.format p;
              Some p
            with
            | _ -> None)
      | _ -> assert false
      
let lids_in_open_paths env lids opens =
  concat_map (lids_in_open_path env lids) opens

let exclude_gen_vars loc ty =
  if gen_vars ty <> [] then
    errorf "%a: overloaded value has a generalized type: %a" Location.print_loc loc Printtyp.type_scheme ty;

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let resolve env cands ty loc = 
    match resolve env cands [([],ty)] with
    | [] -> errorf "%a: no instance found for %a" Location.print_loc loc Printtyp.type_expr ty;
    | [[e]] -> e
    | _ -> errorf  "%a: overloaded type has a too ambiguous type: %a" Location.print_loc loc Printtyp.type_expr ty

  let forge3 env loc ty =
    exclude_gen_vars loc ty;

    (* Get the M of type (...) ...M.name *)
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
    flip iter opens (Format.eprintf "open %a@." Path.format);

    let paths = sort_uniq compare & lids_in_open_paths env [Lident n] (None :: map (fun x -> Some x) opens) in
    iter (fun p -> Format.eprintf "found %a@." Path.format p) paths;

    let cands = flatten & flip map paths & fun path ->
      match 
        try Some (Env.find_module path env) with _ -> None
      with
      | None -> 
          errorf "%a: no module desc found: %a" Location.print_loc loc Path.format path
      | Some mdecl -> get_candidates env (Untypeast.lident_of_path path) mdecl.md_type
    in
    resolve env cands ty loc

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
          exp_desc= Texp_apply (f, map (resolve_arg f.exp_loc e.exp_env) args) }
    | e -> e

  let enter_expression e = match is_imp e with
    | None -> app e

    | Some (`Imp1 lids) ->
        let ty = e.exp_type in
        let env = e.exp_env in
        exclude_gen_vars e.exp_loc ty;
        let cands = flatten & flip map lids & fun lid ->
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
                  errorf "%a: no module desc found: %a" Location.print_loc e.exp_loc Path.format path
              | Some mdecl -> get_candidates e.exp_env lid mdecl.md_type
        in
        resolve env cands ty e.exp_loc 

    | Some (`Imp2 lids) ->
        let ty = e.exp_type in
        let env = e.exp_env in
        exclude_gen_vars e.exp_loc ty;

        let opens = get_opens & Env.summary env in
        flip iter opens (Format.eprintf "open %a@." Path.format);

        let paths = sort_uniq compare & lids_in_open_paths env lids (None :: map (fun x -> Some x) opens) in
        iter (fun p -> Format.eprintf "found %a@." Path.format p) paths;

        let cands = flatten & flip map paths & fun path ->
          match 
            try Some (Env.find_module path env) with _ -> None
          with
          | None -> 
              errorf "%a: no module desc found: %a" Location.print_loc e.exp_loc Path.format path
          | Some mdecl -> get_candidates e.exp_env (Untypeast.lident_of_path path) mdecl.md_type
        in

        resolve env cands ty e.exp_loc 

    | Some `Imp3 ->
        forge3 e.exp_env e.exp_loc e.exp_type
end

module Map = TypedtreeMap.MakeMap(MapArg)
