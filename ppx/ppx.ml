(* Main *)

open Utils
open Ast_mapper

let preconvert = Pre.extend default_mapper
let preconvert_structure = preconvert.structure preconvert 
let preconvert_signature = preconvert.signature preconvert 
  
(* If the tool is ocamldep, we do not type-check *)
let mapper = 
  let structure x str =
    Clflags.dont_write_files := true;
    Warnings.parse_options false "a"; (* print warning *)
    Warnings.parse_options true "a";  (* warning as error *)
    match Ast_mapper.tool_name () with
    | "ocamldep" -> default_mapper.structure x str
    | _ -> 
        Compile.implementation 
          Format.err_formatter 
          "papa" (* dummy *)
          (preconvert_structure str) 
          "gaga" (* dummy *)
  in 
  let signature x sg =
    Clflags.dont_write_files := true;
    Warnings.parse_options false "a"; (* print warning *)
    Warnings.parse_options true "a";  (* warning as error *)
    match Ast_mapper.tool_name () with
    | "ocamldep" -> default_mapper.signature x sg
    | _ -> 
        Compile.interface 
          Format.err_formatter 
          "papa" (* dummy *)
          (preconvert_signature sg) 
          "gaga" (* dummy *)
  in
  { default_mapper with structure; signature }

let () = Ppxx.run "ppx_implicits" mapper
