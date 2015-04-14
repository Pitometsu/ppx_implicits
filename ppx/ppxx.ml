open Parsetree
open Asttypes
open Ast_mapper
open Ast_helper
open Location

(* (@@) is too strong *)
external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"

let (!!%) = Format.eprintf

module Option = struct
  let map f = function
    | None -> None
    | Some v -> Some (f v)
end

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

let test mapper fname = 
  try
    if Filename.check_suffix fname ".ml" then (* .mlt ? *)
      let str = Pparse.parse_implementation ~tool_name:"ppx" Format.err_formatter fname in
      let str = mapper.structure mapper str in
      Pprintast.structure Format.std_formatter str
    else if Filename.check_suffix fname ".mli" then 
      let sg = Pparse.parse_interface ~tool_name:"ppx" Format.err_formatter fname in
      let sg = mapper.signature mapper sg in
      Pprintast.signature Format.std_formatter sg
    else assert false;
    Format.fprintf Format.std_formatter "@."
  with
  | Syntaxerr.Error e ->
      !!% "%a@." Syntaxerr.report_error e

let run name mapper = 
  ppx_name := name;
  let debug = ref false in
  let rev_files = ref [] in 
  Arg.parse 
    [ "-debug", Arg.Set debug, "debug mode which can take .ml/.mli then print the result"
    ]
    (fun s -> rev_files := s :: !rev_files) (* will be handled by [Ast_mapper.apply] *)
    name;
  try
    match !debug, List.rev !rev_files with
    | true, files ->
        List.iter (test mapper) files
    | false, [infile; outfile] ->
        Ast_mapper.apply ~source:infile ~target:outfile mapper
    | _ -> 
        failwith & name ^ " infile outfile"
  with
  | Location.Error e -> Location.report_error Format.err_formatter e
  
