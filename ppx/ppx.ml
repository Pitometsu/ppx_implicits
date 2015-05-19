(* Main *)

open Utils
open Ast_mapper

let preconvert = Pre.extend default_mapper
let preconvert_structure = preconvert.structure preconvert 
let preconvert_signature = preconvert.signature preconvert 
  
(* If the tool is ocamldep, we do not type-check *)
let mapper = { 
  default_mapper with
    structure = (fun x str ->
    Clflags.dont_write_files := true;
    Warnings.parse_options false "a";
    Warnings.parse_options true "a";
    match Ast_mapper.tool_name () with
    | "ocamldep" -> default_mapper.structure x str
    | _ -> Compile.implementation Format.err_formatter "papa" (preconvert_structure str) "gaga");
  signature = (fun x sg -> 
    Clflags.dont_write_files := true;
    Warnings.parse_options false "a";
    Warnings.parse_options true "a";
    match Ast_mapper.tool_name () with
    | "ocamldep" -> default_mapper.signature x sg
    | _ -> Compile.interface Format.err_formatter "papa" (preconvert_signature sg) "gaga"); 
}

let () = Ppxx.run "ppx_implicits" mapper
