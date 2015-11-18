(* Main *)

open Ast_mapper

module M = Typpx.Make.F(struct
  let tool_name = "ppx_implicit"
  let args = []
  let firstUntypedTransformation = Pre.extend default_mapper
  module Typemod = Typpx.Default.Typemod
  module TypedTransformation = Mod.Map
  let lastUntypedTransformation = Typpx.Default.untyped_identity
end)

let () = M.run ()

(*
let run name mapper = 
  ppx_name := name;

  let module Options = Compilerlib.BytecompOptions(struct
    let impl = impl mapper
    let intf = intf mapper
    let anonymous = anonymous mapper
  end) in
  let inappropriate =
    [ "-a"
    ; "-c"
    ; "-cc"
    ; "-cclib"
    ; "-ccopt"
    ; "-compat-32"
    ; "-custom"
    ; "-dllib"
    ; "-dllpath"
    ; "-for-pack"
    ; "-g"
    (* ; "-i" *)
    ; "-linkall"
    ; "-make-runtime"
    ; "-make_runtime"
    ; "-noassert"
    ; "-noautolink"
    ; "-o"
    ; "-output-obj"
    ; "-pack"
    ; "-pp"
    ; "-ppx"
    ; "-runtime-variant"
    ; "-use-runtime"
    ; "-use_runtime"
    ; "-where"
    ; "-"
    ; "-nopervasives"
    ; "-use-prims"
    ; "-drawlambda"
    ; "-dlambda"
    ; "-dinstr"
    ]
  in
    
  let option_list = List.filter (fun (n,_,_) ->
    not & List.mem n inappropriate) Options.list
  in
  
  let debug = ref false in
  let rev_files = ref [] in 
  Arg.parse 
    ([ "-debug", Arg.Set debug, "debug mode which can take .ml/.mli then print the result"
     ; "-debug-pre", Arg.Set debug_pre, "debug pre-preprocessing"
     ; "-debug-resolve", Arg.Set debug_resolve, "debug mode to print overload resolution"
     ; "-debug-unif", Arg.Set debug_unif, "debug mode to print unification results"
     ] @ option_list)
    (fun s -> rev_files := s :: !rev_files) (* will be handled by [Ast_mapper.apply] *)
    name;
  try
    match !debug, List.rev !rev_files with
    | true, files ->
        List.iter (anonymous mapper) files
    | false, [infile; outfile] ->
        Ast_mapper.apply ~source:infile ~target:outfile mapper
    | _ -> 
        failwith & name ^ " infile outfile"
  with
  | Location.Error e -> Location.report_error Format.err_formatter e
  
let () = Ppxx.Ppx.run "ppx_implicits" mapper
  *)
