open Utils

open Ppxx.Utils
open List

open Typpx.Compilerlib
open Path
open Types

open Candidate

(* type M.__imp_spec__ must exists and it is "typeclass"
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
