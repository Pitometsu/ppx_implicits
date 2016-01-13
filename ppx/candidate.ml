(*

  Instance search space specification DSL, magling to and back from
  OCaml type definitions.

*)
open Ppxx.Utils
open List

open Ppxx.Compilerlib
open Longident
open Path
open Types
open Asttypes

open Typpx

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

let rec values_of_module ~recursive env lid path mdecl : t list =
  let m = new Utils.dummy_module env path mdecl.md_type in
  let sg = Utils.scrape_sg path env mdecl in
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
      let sg = Utils.scrape_sg open_ env mdecl in
      let env = Env.open_signature Fresh open_ sg Env.empty in
      flip filter_map lids (fun lid ->
        try
          Some (Env.lookup_module ~load:true (*?*) lid env)
        with
        | _ -> None)
      
let check_module env loc path =
  match 
    try Some (Env.find_module path env) with _ -> None
  with
  | None -> 
      errorf "%a: no module desc found: %a" Location.format loc Path.format path
  | Some mdecl -> mdecl

let cand_direct env loc (flg,lid,popt) =
  let recursive = match flg with
    | `Just -> false
    | `In -> true
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
    let lid = Untypeast.lident_of_path path in
    cand_direct env loc
      & match flg with
      | `Just -> `Just, lid, Some path
      | `In -> `In, lid, Some path) paths


let cand_name rex f =
  f () |> filter (fun x -> Re_pcre.pmatch ~rex & Longident.to_string x.lid)