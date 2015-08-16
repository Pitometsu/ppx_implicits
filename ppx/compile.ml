(* Patched driver/compiler.ml, the heart of Typeful PPX *)

(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The batch compiler *)

open Format
open Typedtree
open Compenv

(* Compile a .mli file *)

(* Keep in sync with the copy in optcompile.ml *)

let tool_name = "ocamlc"

let interface ppf sourcefile ast outputprefix =
  Compmisc.init_path false;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let initial_env = Compmisc.initial_env () in
  (* PPX got AST from the host compiler
  let ast = Pparse.parse_interface ~tool_name ppf sourcefile in
  *)
  if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
  if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.signature ast;

(*
  (* HACK: Save the parsed result as xxx.mli1 *)
  let mli1file = outputprefix ^ ".mli1" in
  let oc1 = open_out_bin mli1file in
  let ppf = formatter_of_out_channel oc1 in
  fprintf ppf "%a@." Pprintast.signature ast;
  close_out oc1;
  (* HACK END *)
*)

  let do_type ast = (* We type twice *)

  let tsg = Typemod.type_interface initial_env ast in
  if !Clflags.dump_typedtree then fprintf ppf "%a@." Printtyped.interface tsg;
  let sg = tsg.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env initial_env (fun () ->
        fprintf std_formatter "%a@."
          Printtyp.signature (Typemod.simplify_signature sg));
  ignore (Includemod.signatures initial_env sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  (*
  if not !Clflags.print_types then begin
    let sg = Env.save_signature sg modulename (outputprefix ^ ".cmi") in
    Typemod.save_signature modulename tsg outputprefix sourcefile
      initial_env sg ;
  end
  *)
  tsg, sg

  in

  (* HACK: typing + mod + untype + retype *)
  let tsg, _sg = do_type ast in
  let tsg = Mod.Map.map_signature tsg in
  (* Untype then save it as xxx.mli2 *)
  let ast = Untypeast.untype_signature tsg in
(*
  let mli2file = outputprefix ^ ".mli2" in
  let oc2 = open_out_bin mli2file in
  let ppf = formatter_of_out_channel oc2 in
  fprintf ppf "%a@." Pprintast.signature ast;
  close_out oc2;
*)
  ast


(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let implementation ppf sourcefile ast outputprefix =
  Compmisc.init_path false;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  (*
  if !Clflags.print_types then begin
    let comp ast =
      ast
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      ++ Typemod.type_implementation sourcefile outputprefix modulename env
      ++ print_if ppf Clflags.dump_typedtree
          Printtyped.implementation_with_coercion
      ++ (fun _ -> ());
      Warnings.check_fatal ();
      Stypes.dump (Some (outputprefix ^ ".annot"))
    in
    try comp (Pparse.parse_implementation ~tool_name ppf sourcefile)
    with x ->
      Stypes.dump (Some (outputprefix ^ ".annot"));
      raise x
  end else *) begin
    (* We do not produce object files.
    let objfile = outputprefix ^ ".cmo" in
    let oc = open_out_bin objfile in
    *)
    let comp ast =
      ast
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure

(*
      (* HACK: Save the parsed result as xxx.ml1 *)
      ++ (fun ptree -> 
        let ml1file = outputprefix ^ ".ml1" in
        let oc1 = open_out_bin ml1file in
        let ppf = formatter_of_out_channel oc1 in
        fprintf ppf "%a@." Pprintast.structure ptree; 
        close_out oc1;
        ptree
      )
      (* HACK END *)
*)

      ++ Typemod.type_implementation sourcefile outputprefix modulename env
      ++ print_if ppf Clflags.dump_typedtree
                  Printtyped.implementation_with_coercion

      (* HACK: typed mod *)
      ++ (fun (str, mc) -> Mod.Map.map_structure str, mc)
      (* HACK END *)

      (* HACK: untype + retype *)
      ++ (fun (str, _) -> 
        (* Untype then save it as xxx.ml2 *)
        let ast =  Untypeast.untype_structure str in
(*
        let ml2file = outputprefix ^ ".ml2" in
        let oc2 = open_out_bin ml2file in
        let ppf = formatter_of_out_channel oc2 in
        fprintf ppf "%a@." Pprintast.structure ptree; 
        close_out oc2;
*)
        ast)

      (*
      ++ Translmod.transl_implementation modulename
      ++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
      ++ Simplif.simplify_lambda
      ++ print_if ppf Clflags.dump_lambda Printlambda.lambda
      ++ Bytegen.compile_implementation modulename
      ++ print_if ppf Clflags.dump_instr Printinstr.instrlist
      ++ Emitcode.to_file oc modulename objfile;
      Warnings.check_fatal ();
      close_out oc;
      Stypes.dump (Some (outputprefix ^ ".annot"))
      *)
      ++ fun ast -> 
        Stypes.dump (Some (outputprefix ^ ".annot"));
        ast
    in
    (* PPX takes AST from the host compiler *)
    try comp ast (* (Pparse.parse_implementation ~tool_name ppf sourcefile) *)
    with x ->
      (*
      close_out oc;
      remove_file objfile;
      *)
      Stypes.dump (Some (outputprefix ^ ".annot"));
      raise x
  end
