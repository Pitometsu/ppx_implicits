(*

  Instance search space specification DSL, magling to and back from
  OCaml type definitions.

*)
open Utils

open Ppxx.Utils
open List

open Ppxx.Compilerlib
open Longident
open Path
open Types

open Typpx

open Asttypes

type t = {
  lid        : Longident.t;
  path       : Path.t;
  expr       : Typedtree.expression;
  type_      : Types.type_expr;
  aggressive : bool
}

let uniq xs =
  let tbl = Hashtbl.create 107 in
  iter (fun x ->
    try
      let x' = Hashtbl.find tbl x.path in
      Hashtbl.replace tbl x.path { x with aggressive = x.aggressive || x'.aggressive }
    with
    | Not_found -> Hashtbl.add tbl x.path x) xs;
  Hashtbl.to_list tbl |> map snd

let scrape_sg _path env mdecl = 
  try
    match Env.scrape_alias env & Mtype.scrape env mdecl.md_type with
    | Mty_signature sg ->
(*
        test_scrape_sg path env sg;
*)
        sg
    | Mty_functor _ -> [] (* We do not scan the internals of functors *)
    | _ -> assert false
  with
  | e -> 
      !!% "scraping failed: %s" & Printexc.to_string e;
      raise e

let _test_scrape_sg path env sg =
  match path with
  | Pident {Ident.name = "Pervasives"} -> ()
  | _ ->
  let lid = Typpx.Untypeast.lident_of_path path in      
  !!% "SCRAPE SG %a@." Path.format_verbose path;
  flip iter sg & function
    | Sig_value (id, _vdesc) ->
        let lid = Ldot (lid, id.Ident.name) in
        let popt = try Some (fst (Env.lookup_value lid env)) with _ -> None in
        !!% "  value %a  >> %a@." Ident.format_verbose id (Option.format Path.format_verbose) popt
    | Sig_module (id, _moddecl, _) ->
        !!% "  module %a@." Ident.format_verbose id
    | Sig_type (id, _, _) -> 
        !!% "  type %a@." Ident.format_verbose id
    | _ -> ()
    
let rec values_of_module ~recursive env lid path mdecl : t list =
  let m = new Utils.dummy_module env path mdecl.md_type in
  let sg = scrape_sg path env mdecl in
  flip2 fold_right sg [] & fun sitem st -> match sitem with
  | Sig_value (id, _vdesc) ->
      let lid = Ldot (lid, Ident.name id) in
      let path = try m#lookup_value & Ident.name id with Not_found ->
        !!% "VOM m#lookup_value %s not found@." & Ident.name id;
        assert false
      in
      begin
        try
          let type_ = (Env.find_value path env).val_type in
          let expr = Forge.Exp.(with_env env & ident path) in
          (* !!% "    VOM: %a@." Path.format_verbose path; *)
          { lid; path; expr; type_; aggressive= false }  :: st
        with
        | Not_found ->
            !!% "VOM: %a but not found@." Path.format_verbose path;
            assert false
      end
  | Sig_module (id, moddecl, _) when recursive -> 
      let lid = Ldot (lid, Ident.name id) in
      let path = m#lookup_module & Ident.name id in
      values_of_module ~recursive env lid path moddecl @ st
        
  | _ -> st

let get_opens env =
  let rec get = function
    | Env.Env_empty -> []
    | Env_value (s, _, _)
    | Env_type (s, _, _)
    | Env_extension (s, _, _)
    | Env_module (s, _, _)
    | Env_modtype (s, _, _)
    | Env_class (s, _, _)
    | Env_cltype (s, _, _)
    | Env_functor_arg (s, _) -> get s
    | Env_open (s, path) -> path :: get s
  in
  get & Env.summary env

let _dump_summary env =
  let rec dump = function
    | Env.Env_empty -> ()
    | Env_value (s, _, _)
    | Env_extension (s, _, _)
    | Env_modtype (s, _, _)
    | Env_class (s, _, _)
    | Env_cltype (s, _, _)
    | Env_functor_arg (s, _) -> dump s
    | Env_type (s, id, _) -> !!% "type %a@." Ident.format id; dump s
    | Env_module (s, id, _) -> !!% "module %a@." Ident.format id; dump s
    | Env_open (s, path) -> !!% "open %a@." Path.format path; dump s
  in
  dump & Env.summary env
  
let module_lids_in_open_path env lids = function
  | None -> 
      (* Finds lids in the current scope, but only defined ones in the current scope level.
           * Persistent ones are excluded
           * Sub-modules are excluded
      *)
      flip filter_map lids (fun lid ->
        try
          let p = Env.lookup_module ~load:true (*?*) lid env in
          match p with
          | Pident id when not & Ident.persistent id -> Some p
          | _ -> None (* not sure... *)
        with
        | _ -> None)
  | Some open_ ->
      (*  !!% "open %a@." Path.format open_; *)
      let mdecl = Env.find_module open_ env in (* It should succeed *)
      let sg = scrape_sg open_ env mdecl in
      let env = Env.open_signature Fresh open_ sg Env.empty in
      flip filter_map lids (fun lid ->
        try
          Some (Env.lookup_module ~load:true (*?*) lid env)
        with
        | _ -> None)
      
let data_types env ty =
  let open Btype in
  let open Ctype in
  let res = ref [] in
  (* CR jfuruse: oops, may loop forever? *)
  let expand_repr_desc env ty = (repr & expand_head env ty).desc in
  let rec loop ty = 
    begin match expand_repr_desc env ty with
    | Tconstr (p, _tys, _) ->
        res := p :: !res;
    | _ -> ()
    end;
    iter_type_expr loop ty
  in
  loop ty;
  sort_uniq compare !res

let related_modules env ty =
  let open Path in
  data_types env ty
  |> filter_map (function
      | Pdot (p, _, _) -> Some p
      | Pident _ -> None
      | Papply _ -> assert false)
  |> sort_uniq compare

let check_module env loc path =
  match 
    try Some (Env.find_module path env) with _ -> None
  with
  | None -> 
      errorf "%a: no module desc found: %a" Location.format loc Path.format path
  | Some mdecl -> mdecl

let cand_direct env loc (flg,lid,popt) =
  let recursive, lid, popt = match flg with
    | `Just -> false, lid, popt
    | `In -> true, lid, popt
  in
  let path = match popt with
    | Some p -> p
    | None -> 
        try
          Env.lookup_module ~load:true lid env
        with
        | Not_found -> errorf "%a: Unbound module %a." Location.format loc Longident.format lid
  in
  let mdecl = check_module env loc path in
  values_of_module ~recursive env lid path mdecl

(* CR jfuruse: loc is not used *)    
let cand_related env _loc ty = 
  let mods = related_modules env ty in
  let lmods = flip map mods & fun p ->
    let mdecl = Env.find_module p env in
    (Typpx.Untypeast.lident_of_path p, p, mdecl)
  in
  (* CR jfuruse: values_of_module should be memoized *)
  concat & map (fun (lid,path,mdecl) -> values_of_module ~recursive:false env lid path mdecl) lmods
  
let cand_opened env loc (flg,lid) =
  let opens = get_opens env in
  if !Options.debug_resolve then begin
    !!% "debug_resolve: cand_opened opened paths@.";
    flip iter opens & !!% "  %a@." Path.format
  end;
  let paths = 
    concat 
    & map (module_lids_in_open_path env [lid]) 
    & None :: map (fun x -> Some x) opens
  in
  if !Options.debug_resolve then begin
    !!% "debug_resolve: cand_opened cand modules@.";
    flip iter paths & !!% "  %a@." Path.format
  end;
  concat & map (fun path ->
    let lid = Typpx.Untypeast.lident_of_path path in
    cand_direct env loc
      & match flg with
      | `Just -> `Just, lid, Some path
      | `In -> `In, lid, Some path) paths

(* [%imp] : 'a M.ty
   type M.__imp_spec__ must exists and it is "typeclass"
   We seek types equal to __imp_instance__ = M.__imp_spec__
*) 
let cand_typeclass env loc p_spec =
  let has_instance mp =
    let md = Env.find_module mp env in
    let m = new dummy_module env mp md.md_type in
    try
      let _, td = m#lookup_type "__imp_instance__" in
      match td with
      | { type_params = []
        ; type_manifest = Some { desc = Tconstr (p, _, _) } } when p = p_spec ->
          Some mp
          (* Some (Pdot (mp, "__imp_instance__", n)) *)
      | _ -> None
    with
    | Not_found -> None
  in
  let rec find_modules = function
    | Env.Env_empty -> []
    | Env_value (s, _, _)
    | Env_extension (s, _, _)
    | Env_modtype (s, _, _)
    | Env_class (s, _, _)
    | Env_cltype (s, _, _)
    | Env_functor_arg (s, _)
    | Env_type (s, _, _) -> find_modules s
    | Env_module (s, id, _md) ->
        let res = find_modules s in
        begin match has_instance (Pident id) with
        | None -> res
        | Some x -> x :: res
        end
    | Env_open (s, path) ->
        let md = Env.find_module path env in
        (* Strange way to ask the correct position ... *)
        let m = new dummy_module env path md.md_type in
        fold_left (fun res -> function
          | Sig_module (id, _md, _) ->
              begin match has_instance (m#lookup_module id.Ident.name) with
              | None -> res
              | Some x -> x :: res
              end
          | _ -> res)
          (find_modules s) (scrape_sg path env md)
  in
  let paths = find_modules & Env.summary env in
  if !Options.debug_resolve then begin
    !!% "debug_resolve: cand_typeclass cand modules@.";
    flip iter paths & !!% "  %a@." Path.format
  end;
  concat & map (fun path ->
    let lid = Typpx.Untypeast.lident_of_path path in
    cand_direct env loc (`Just, lid, Some path)) paths
    
let cand_name rex f =
  filter (fun x ->
    Re_pcre.pmatch ~rex & Longident.to_string x.lid) & f ()
