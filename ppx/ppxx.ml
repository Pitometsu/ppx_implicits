(* PPX related tools *)

open Utils

open Parsetree
open Asttypes
open Ast_mapper
open Ast_helper
open Location

let at ?loc txt = 
  let loc = match loc with 
      | None -> !Ast_helper.default_loc
      | Some loc -> loc
  in
  { txt; loc }

 
let lid ?loc s = at ?loc & Longident.parse s

let with_default_loc loc f = 
  let back = !Ast_helper.default_loc in
  Ast_helper.default_loc := loc;
  let res = f () in
  Ast_helper.default_loc := back;
  res

module Name = struct
  let make_unique =
    let cntr = ref 0 in
    fun n ->
      let x = !cntr in
      incr cntr;
      n ^ "_" ^ string_of_int x
end

module Longident = struct
  include Longident
  let format = Pprintast.default#longident
end

module Ident = struct
  include Ident
  let format ppf id = Format.fprintf ppf "%s/%d" id.name id.stamp
end

module Path = struct
  include Path

  let rec format_verbose ppf =
    let open Format in
    function
      | Pident id -> Ident.format ppf id
      | Pdot (p, name, n) -> fprintf ppf "%a.%s__%d" format_verbose p name n
      | Papply (p1, p2) -> fprintf ppf "%a(%a)" format_verbose p1 format_verbose p2

  let rec format ppf =
    let open Format in
    function
      | Pident id -> Ident.format ppf id
      | Pdot (p, name, _n) -> fprintf ppf "%a.%s" format p name
      | Papply (p1, p2) -> fprintf ppf "%a(%a)" format p1 format p2
end
  
module Typ = struct
  include Typ
  let new_var =
    let cntr = ref 0 in
    fun () -> 
      incr cntr;
      var & "tvar_" ^ string_of_int !cntr
  let ref_ ?loc ?attrs ty = 
    constr ?loc ?attrs (at ?loc & Longident.Lident "ref") [ty]
end

module Exp = struct
  include Exp
  let var ?loc ?attrs s = ident ?loc ?attrs & at ?loc & Longident.Lident s
  let string ?loc ?attrs s = constant ?loc ?attrs & Const_string (s, None)
  let int ?loc ?attrs i = constant ?loc ?attrs & Const_int i
  let bool ?loc ?attrs b = construct ?loc ?attrs (lid ?loc (if b then "true" else "false")) None
  let id ?loc ?attrs s = ident ?loc ?attrs & at ?loc & Longident.parse s
  let option ?loc ?attrs = function
    | None -> construct ?loc ?attrs (lid ?loc "None") None
    | Some e -> construct ?loc ?attrs (lid ?loc "Some") (Some e)
  let parse s =
    try
      Parser.parse_expression Lexer.token (Lexing.from_string s)
    with
    | _e -> failwith (Printf.sprintf "parse fail: %s" s)

  let object_ ?loc ?attrs flds = object_ ?loc ?attrs (Cstr.mk (Pat.any ()) flds)
  let seqs = function
    | [] -> assert false
    | x::xs -> List.fold_right (fun x st -> sequence x st) xs x
  let ignore_ e = apply (id "Pervasives.ignore") ["", e]
  let assert_false () = assert_ & bool false
end

module Pat = struct
  include Pat
  let var ?loc ?attrs s = var ?loc ?attrs (at ?loc s)

  exception Not_supported of expression

  let of_expr e = 
    let rec f e = 
      let loc = e.pexp_loc in
      let attrs = e.pexp_attributes in
      match e.pexp_desc with
      | Pexp_ident {txt=Lident s; loc=loc'} -> 
          Pat.var ~loc ~attrs {txt=s; loc=loc'} 
      | Pexp_constant c -> Pat.constant ~loc ~attrs c
      | Pexp_tuple es -> Pat.tuple ~loc ~attrs & List.map f es
      | Pexp_construct (lid, eopt) ->
          Pat.construct ~loc ~attrs lid & Option.map f eopt
      | Pexp_variant (l, eopt) ->
          Pat.variant ~loc ~attrs l & Option.map f eopt
      | Pexp_record (fields , None) ->
          Pat.record ~loc ~attrs (List.map (fun (lid, e) -> lid, f e) fields) Closed
      | Pexp_array es -> Pat.array ~loc ~attrs & List.map f es
      | Pexp_constraint (e, ty) -> Pat.constraint_ ~loc ~attrs (f e) ty
      | Pexp_lazy e -> Pat.lazy_ ~loc ~attrs & f e
      | Pexp_extension ({txt="p"}, PPat (p, None)) -> p
      | _ -> raise (Not_supported e)
    in
    try
      `Ok (f e)
    with
    | Not_supported e -> `Error e
end

module ExpPat = struct
  let var ?loc ?attrs s = (Exp.var ?loc ?attrs s, Pat.var ?loc ?attrs s)
end

module Cf = struct
  include Cf

  let method_concrete ?loc ?attrs name ?(priv=false) ?(override=false) e = 
    Cf.method_ ?loc ?attrs name (if priv then Private else Public)
      (Cfk_concrete ((if override then Override else Fresh), e))
  let method_virtual ?loc ?attrs name ?(priv=false) cty = 
    Cf.method_ ?loc ?attrs name (if priv then Private else Public)
      (Cfk_virtual cty)
end

module Cstr = struct
  let mk ?(self= Pat.any ()) fields = Cstr.mk self fields
end

let ppx_name = ref "ppx name is not set"

let handle_error f =
  try f () with 
  | Syntaxerr.Error e ->
      !!% "%a@." Syntaxerr.report_error e;
      exit 2
  | e ->
      Format.eprintf "%a@."  Location.report_exception e;
      exit 2
  
    
let impl mapper fname =
  handle_error & fun () ->
    let str = Pparse.parse_implementation ~tool_name:"ppx" Format.err_formatter fname in
    let str = mapper.structure mapper str in
    Pprintast.structure Format.std_formatter str;
    Format.fprintf Format.std_formatter "@."

let intf mapper fname =
  handle_error & fun () ->
    let sg = Pparse.parse_interface ~tool_name:"ppx" Format.err_formatter fname in
    let sg = mapper.signature mapper sg in
    Pprintast.signature Format.std_formatter sg;
    Format.fprintf Format.std_formatter "@."
    
let anonymous mapper fname = 
  if Filename.check_suffix fname ".ml" then impl mapper fname (* .mlt ? *)
  else if Filename.check_suffix fname ".mli" then intf mapper fname 
  else assert false

let debug_resolve = ref false
let debug_unif = ref false

let run name mapper = 
  ppx_name := name;

  let module Options = Main_args.Make_bytecomp_options (struct
open Clflags
open Compenv

let show_config () =
  Config.print_config stdout;
  exit 0;
;;
  let set r () = r := true
  let unset r () = r := false
  let _a = set make_archive
  let _absname = set Location.absname
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _cc s = c_compiler := Some s
  let _cclib s = ccobjs := Misc.rev_split_words s @ !ccobjs
  let _ccopt s = first_ccopts := s :: !first_ccopts
  let _compat_32 = set bytecode_compatible_32
  let _config = show_config
  let _custom = set custom_runtime
  let _dllib s = dllibs := Misc.rev_split_words s @ !dllibs
  let _dllpath s = dllpaths := !dllpaths @ [s]
  let _for_pack s = for_package := Some s
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I s = include_dirs := s :: !include_dirs
  let _impl = impl mapper
  let _intf = intf mapper
  let _intf_suffix s = Config.interface_suffix := s
  let _keep_locs = set keep_locs
  let _labels = unset classic
  let _linkall = set link_everything
  let _make_runtime () =
    custom_runtime := true; make_runtime := true; link_everything := true
  let _no_alias_deps = set transparent_modules
  let _no_app_funct = unset applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noautolink = set no_auto_link
  let _nostdlib = set no_std_include
  let _o s = output_name := Some s
  let _open s = open_modules := s :: !open_modules
  let _output_obj () = output_c_object := true; custom_runtime := true
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _ppx s = first_ppx := s :: !first_ppx
  let _principal = set principal
  let _rectypes = set recursive_types
  let _runtime_variant s = runtime_variant := s
  let _safe_string = unset unsafe_string
  let _short_paths = unset real_paths
  let _strict_sequence = set strict_sequence
  let _strict_formats = set strict_formats
  let _thread = set use_threads
  let _vmthread = set use_vmthreads
  let _unsafe = set fast
  let _unsafe_string = set unsafe_string
  let _use_prims s = use_prims := s
  let _use_runtime s = use_runtime := s
  let _v () = print_version_and_library "compiler"
  let _version = print_version_string
  let _vnum = print_version_string
  let _w = (Warnings.parse_options false)
  let _warn_error = (Warnings.parse_options true)
  let _warn_help = Warnings.help_warnings
  let _where = print_standard_library
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr
  let anonymous = anonymous mapper
end)
  in
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
  
