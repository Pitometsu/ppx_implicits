(* PPX related tools *)

open Utils

open Parsetree
open Asttypes
open Ast_mapper
open Ast_helper
open Location

let ghost l = { l with loc_ghost = true }
    
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

module Typ = struct
  include Typ
  let new_var =
    let cntr = ref 0 in
    fun () -> 
      incr cntr;
      var & "tvar_" ^ string_of_int !cntr
  let ref_ ?loc ?attrs ty = 
    constr ?loc ?attrs (at ?loc & Longident.Lident "ref") [ty]
  let option ?loc ?attrs ty =
    constr ?loc ?attrs (at ?loc & Longident.(Ldot (Lident "*predef*", "option"))) [ty]
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
  include Cstr
  let mk ?(self= Pat.any ()) fields = Cstr.mk self fields
end

module Mod = struct
  include Ast_helper.Mod
  let ident' ?loc lid = ident ?loc (at ?loc lid)
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

let debug_pre = ref false
let debug_resolve = ref false
let debug_unif = ref false

let run name mapper = 
  ppx_name := name;

  let module Options = Compilerlibx.BytecompOptions(struct
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
  
