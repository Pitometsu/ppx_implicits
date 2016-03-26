(*

  Pre-preprocessing for syntax sugars for 
    [%%imp_spec]
*)

open Ppxx.Utils
open Ppxx.Compilerlib

open Parsetree
open Asttypes
open Ast_mapper

(* type __imp_spec__ = private Spec_xxxx *)
let extend super =
  let do_imp_spec loc pld f = match Specconv.from_payload Env.empty (* dummy *) pld with
    | `Error err -> Specconv.error loc err
    | `Ok Spec.Type ->
        errorf "%a: [%%%%imp_spec SPEC] requires a SPEC expression"
          Location.format loc
    | `Ok spec -> f spec
  in

  let typ self cty =
    match cty.ptyp_desc with
    | Ptyp_extension ({txt="imp_spec"}, pld) ->
        do_imp_spec cty.ptyp_loc pld & fun spec ->
          Specconv.to_core_type cty.ptyp_loc spec 
    | _ -> super.typ self cty
  in
  { super with typ }
