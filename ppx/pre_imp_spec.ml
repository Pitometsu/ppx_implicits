(*

  Pre-preprocessing for syntax sugars for 
    [%imp <spec>]
*)

open Ppxx.Utils

open Parsetree
open Asttypes
open Ast_mapper

let extend super =
  let do_imp loc pld f = match Specconv.from_payload Env.empty (* dummy *) pld with
    | Error err -> Specconv.error loc err
    | Ok spec -> f spec
  in

  let typ self cty =
    match cty.ptyp_desc with
    | Ptyp_extension ({txt="imp"}, pld) ->
        do_imp cty.ptyp_loc pld & fun spec ->
          Specconv.to_core_type cty.ptyp_loc spec 
    | _ -> super.typ self cty
  in
  { super with typ }
