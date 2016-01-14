(*

  Instance search space specification DSL, magling to and back from
  OCaml type definitions.

*)
open Ppxx.Utils
open List
open Format

open Ppxx.Compilerlib
open Longident
open Parsetree
open Types

module Forge = Typpx.Forge
  
(** spec dsl *)
type t = 
  | Or of t2 list (** [t2, .., t2] *)
  | Type (** [%imp].  Encoded as a type definition.  No allowed in [%%imp_spec] *)

and t2 = 
  | Opened of [`In | `Just] * Longident.t (** [opened M]. The values defined under module path [P.M] which is accessible as [M] by [open P] *)
  | Direct of [`In | `Just] * Longident.t * Path.t option
    (** [P] or [just P]. [P] is for the values defined under module [P] and [P]'s sub-modules. [just P] is for values defined just under module [P] and values defined in its sub-modules are not considered. *)
  | Aggressive of t2 (** [aggressive t2]. Even normal function arrows are considered as constraints. *)
  | Related (** [related]. The values defined under module [P] where data type defined in [P] appears in the type of the resolution target *)
  | Name of string * Re.re * t2 (** [name "rex" t2]. Constraint values only to those whose names match with the regular expression *)
  | Typeclass of Path.t option (** [typeclass]. Typeclass style resolution. The argument is None at parsing, but must be filled with Some until the resolution. *) 
  | Deriving of Longident.t (** [deriving M]. [M] must define [M.tuple], [M.object_] and [M.poly_variant] *)
  | PPXDerive of Parsetree.expression * core_type * type_expr option (** [ppxderive ([%...] : ty)]. *)
      
let rec is_static = function
  | Opened _ -> true
  | Direct _ -> true
  | Related -> false
  | Aggressive t2 | Name (_, _, t2) -> is_static t2
  | Typeclass _ -> true
  | Deriving _ -> false
  | PPXDerive _ -> false
    
let to_string = 
  let rec t = function
    | Type -> ""
    | Or [] -> assert false
    | Or [x] -> t2 x
    | Or xs -> String.concat ", " (map t2 xs)
  and t2 = function
    | Direct (`In, l, _) -> Longident.to_string l
    | Direct (`Just, l, _) -> Printf.sprintf "just %s" & Longident.to_string l
    | Opened (`In, l) -> Printf.sprintf "opened (%s)" & Longident.to_string l
    | Opened (`Just, l) -> Printf.sprintf "opened (just %s)" & Longident.to_string l
    | Related -> "related"
    | Aggressive x -> Printf.sprintf "aggressive (%s)" (t2 x)
    | Name (s, _re, x) -> Printf.sprintf "name %S (%s)" s (t2 x)
    | Typeclass _ -> "typeclass"
    | Deriving p -> Printf.sprintf "deriving %s" & Longident.to_string p
    | PPXDerive (e, cty, _) ->
        Format.asprintf "ppxderive (%a : %a)"
          Pprintast.expression e
          Pprintast.core_type cty
  in
  t 

let expression_from_string s = 
  let lexbuf = Lexing.from_string s in
  try `Ok (Parser.parse_expression Lexer.token lexbuf) with
  | _ -> `Error (`Parse s)

module Hack = struct
  open Ppxx.Helper.Typ
  open Parsetree

  module Encode = struct
    let string : string -> core_type = fun s ->
      variant [Rtag (Utils.mangle s, [], true, [])] Closed None
        
    let node : string -> core_type list -> core_type = fun s -> function
      | []  -> variant [Rtag (s, [], true, [])] Closed None
      | [x] -> variant [Rtag (s, [], false, [x])] Closed None
      | xs  -> variant [Rtag (s, [], false, [tuple xs])] Closed None
  
    let in_just = function
      | `In -> node "_In" []
      | `Just -> node "_Just" []
  
    let longident l = string & Longident.to_string l
      
    let rec t2 = function
      | Direct (ij, l, _) ->
          node "_Direct" [ in_just ij; longident l]
      | Opened (ij, l) ->
          node "_Opened" [ in_just ij; longident l]
      | Related -> node "_Related" []
      | Aggressive x -> node "_Aggressive" [t2 x]
      | Name (s, _re, x) -> node "_Name" [string s; t2 x]
      | Typeclass _ -> node "_Typeclass" []
      | Deriving l -> node "_Deriving" [longident l]
      | PPXDerive (s, ty, _) -> node "_PPXDerive" [string (Pprintast.string_of_expression s); ty]
  
    let t = function
      | Type -> assert false
      | Or [] -> assert false
      | Or t2s ->
          let xs = map t2 t2s in
          let get_row_fields cty = match cty.ptyp_desc with
            | Ptyp_variant (rfs, _, _) -> rfs
            | _ -> assert false
          in
          variant (concat_map get_row_fields xs) Closed None
  end

  module Decode = struct
    let (>>=) = Utils.(>>=)
    let ok x = `Ok x
      
    let desc cty = cty.ptyp_desc

    let string cty = match desc cty with
      | Ptyp_variant ([Rtag (ms, [], true, [])], Closed, None) ->
          Utils.unmangle ms
      | _ -> `Error (`Expected "string")
        
    let node cty = match desc cty with
      | Ptyp_variant ([Rtag (s, [], true, [])], Closed, None) -> `Ok (s, [])
      | Ptyp_variant ([Rtag (s, [], false, [cty])], Closed, None) ->
          begin match desc cty with
          | Ptyp_tuple ctys -> `Ok (s, ctys)
          | _ -> `Ok (s, [cty])
          end
      | _ -> `Error (`Expected "node")

    let in_just cty = match desc cty with
      | Ptyp_variant ([Rtag ("_In", [], true, [])], Closed, None) -> `Ok `In
      | Ptyp_variant ([Rtag ("_Just", [], true, [])], Closed, None) -> `Ok `Just
      | _ -> `Error (`Expected "In/Just")

    let longident cty = match desc cty with
      | Ptyp_variant ([Rtag (ms, [], true, [])], Closed, None) ->
          Utils.unmangle ms >>= fun s ->
          (try ok & Longident.parse s with _ -> `Error (`Expected "longident"))
      | _ -> `Error (`Expected "longident")

    let rec t2 cty = node cty >>= function
      | "_Direct", [ij; lid] ->
          in_just ij >>= fun ij ->
          longident lid >>= fun lid ->
          ok & Direct (ij, lid, None)
      | "_Direct", _ -> `Error (`SyntaxError "Direct requires 2 args")
      | "_Opened", [ij; lid] ->
          in_just ij >>= fun ij ->
          longident lid >>= fun lid ->
          ok & Opened (ij, lid)
      | "_Opened", _ -> `Error (`SyntaxError "Opened requires 2 args")
      | "_Related", [] -> ok Related
      | "_Related", _ -> `Error (`SyntaxError "Related cannot have args")
      | "_Aggressive", [x] ->
          t2 x >>= fun x -> ok & Aggressive x
      | "_Aggressive", _ -> `Error (`SyntaxError "Aggressive requires 1 arg")
      | "_Name", [s; x] ->
          string s >>= fun s ->
          t2 x >>= fun x ->
          ok & Name (s, Re_pcre.regexp s, x)
      | "_Name", _ -> `Error (`SyntaxError "Name requires 2 args")
      | "_Typeclass", [] -> ok & Typeclass None
      | "_Typeclass", _ -> `Error (`SyntaxError "Typeclass cannot have arg")
      | "_Deriving", [l] ->
          longident l >>= fun l -> ok & Deriving l
      | "_Deriving", _ -> `Error (`SyntaxError "Deriving requires 1 arg")
      | "_PPXDerive", [s; ty] ->
          string s >>= fun s ->
          expression_from_string s >>= fun e ->
          ok & PPXDerive (e, ty, None)
      | "_PPXDerive", _ -> `Error (`SyntaxError "PPXDerive requires 2 args")
      | s, _ -> `Error (`SyntaxError ("Unknown keyword: " ^ s))

    let rec mapM f = function
      | [] -> ok []
      | x::xs ->
          f x >>= fun y ->
          mapM f xs >>= fun ys ->
          ok & y :: ys
          
    let t cty = match desc cty with
      | Ptyp_variant (xs, Closed, None) ->
          begin mapM t2 (map (fun x -> variant [x] Closed None) xs) >>= function
            | [] -> `Error (`SyntaxError "spec requires more than one component")
            | xs -> ok & Or xs
          end
      | _ -> `Error (`SyntaxError "strange spec")
  end
end

(*
let to_core_type =
  let open Ppxx.Helper.Typ in
  let rec t = function
    | Type -> assert false
    | Or [] -> assert false
    | Or xs -> variant (map t2 xs) Closed None
  and t2 = function
    | Direct (`In, l, _) ->
        (* [`_Direct of [`_In] * l]  *)
        
        unary "in" & string & Longident.to_string l
    | Direct (`Just, l, _) -> unary "just" & string & Longident.to_string l
    | Opened (`In, l) -> unary "opened" Printf.sprintf "opened (%s)" & Longident.to_string l
    | Opened (`Just, l) -> Printf.sprintf "opened (just %s)" & Longident.to_string l
    | Related -> "related"
    | Aggressive x -> Printf.sprintf "aggressive (%s)" (t2 x)
    | Name (s, _re, x) -> Printf.sprintf "name %S (%s)" s (t2 x)
    | Typeclass _ -> "typeclass"
    | Deriving p -> Printf.sprintf "deriving %s" & Longident.to_string p
    | PPXDerive (s, _) -> s
  in
  t 
*)
      
let prefix = "Spec_"
let prefix_len = String.length prefix

let to_mangled_string x = prefix ^ Utils.mangle (to_string x)

(* CR jfuruse: need tests *)
let unmangle s = 
  if not & String.is_prefix prefix s then assert false; (* CR jfuruse: better error *)
  let s = String.sub s prefix_len (String.length s - prefix_len) in
  Utils.unmangle s

let from_string = expression_from_string

let from_expression _env e = 
  try
    let get_lid e = match e.pexp_desc with
      | Pexp_construct ({txt=lid}, None) -> Some lid
      | _ -> None
    in
    let rec t e = match e.pexp_desc with
      | Pexp_tuple xs -> Or (map t2 xs)
      | _ -> Or [t2 e]
    and t2 e = match e.pexp_desc with
      | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "aggressive"} },
                    ["", e] ) -> Aggressive (t2 e)
      | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "opened"} },
                    ["", e] ) ->
          let f,l = flag_lid e in Opened (f,l)
      | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "name"} },
                    [ "", { pexp_desc = Pexp_constant (Const_string (s, _)) }
                    ; "", e ] ) -> Name (s, Re_pcre.regexp s, t2 e)
      | Pexp_ident {txt=Lident "related"} -> Related
      | Pexp_ident {txt=Lident "typeclass"} -> Typeclass None
      | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "deriving"} }, args ) ->
          begin match args with
          | ["", e] -> 
              begin match get_lid e with
              | Some lid -> Deriving lid
              | None -> errorf "deriving must take an module path" Location.format e.pexp_loc
              end
          | _ -> errorf "deriving must take just one argument"
          end
      | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "ppxderive"} }, args ) ->
          begin match args with
          | ["", e] ->
              let e, cty = Cppxderive.parse0 e in
              PPXDerive (e, cty, None)
          | _ -> errorf "derive must take just one argument"
          end
      | _ -> 
          let f,lid = flag_lid e in
          Direct (f, lid, None)
    and flag_lid e = match e.pexp_desc with
      | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "just"} },
                    ["", e] ) -> 
          begin match get_lid e with
          | Some lid -> `Just, lid
          | None -> errorf "%a: just requires an argument" Location.format e.pexp_loc
          end
      | Pexp_construct ({txt=lid}, None) -> `In, lid
      | _ ->
          errorf "%a: Illegal spec expression" Location.format e.pexp_loc
    in
    `Ok (t e)
  with
  | Failure s -> `Error (`ParseExp (e, s))

let from_structure env str =
  match str with
  | [] -> `Ok Type
  | _::_::_ -> 
      `Error (`String "multiple implicit policies are not allowed")
  | [sitem] ->
      match sitem.pstr_desc with
      | Pstr_eval (e, _) ->
          from_expression env e
      | _ ->
          `Error (`String "spec must be an OCaml expression")

let error loc = function
  | `String s -> errorf "%a: %s" Location.format loc s
  | `Failed_unmangle s -> 
      errorf "%a: Illegal spec encoding: %S" Location.format loc s
  | `Parse s ->
      errorf "%a: Spec parse failed: %S" Location.format loc s
  | `ParseExp (_, s) ->
      errorf "%a: Spec parse failed: %s" Location.format loc s

let from_ok loc = function
  | `Ok v -> v
  | `Error e -> error loc e

let from_payload env = function
  | PStr s -> from_structure env s
  | _ -> `Error (`String "spec must be an OCaml expression")

(* typed world *)

(** fill Typeclass None *)
let fix_typeclass _loc p = function
  | Type -> assert false
  | Or t2s ->
      let f = function
        | Typeclass None -> Typeclass (Some p)
        | Typeclass (Some _) -> assert false
        | x -> x
      in
      Or (map f t2s)

(* CR jfuruse: >>= for Utils.(>>=) *)    
let from_type_decl env loc p = let open Utils in function
  | { type_params = []
    ; type_kind = Type_variant [ { cd_id= id; cd_args = []; cd_res = None; cd_loc = loc} ]
    ; type_manifest = None } ->
      fix_typeclass loc p
      & from_ok loc
      & unmangle id.Ident.name >>= from_string >>= from_expression env
  | { type_params = _
    ; type_kind = Type_variant [ { cd_id= id; cd_args = _ctys; cd_res = None; cd_loc = loc} ]
    ; type_manifest = None } ->
      let (>>=) x f = match x with `Error e -> `Error e | `Ok v -> f v in
      fix_typeclass loc p
      & from_ok loc
      & unmangle id.Ident.name >>= from_string >>= from_expression env
  | _ -> 
      errorf "%a: Illegal data type definition for __imp_spec__. [%%%%imp_spec SPEC] must be used." Location.format loc

        
let from_module_type env loc mp mty =
  let m = new Utils.dummy_module env mp mty in
  try
    let p, td = m#lookup_type "__imp_spec__" in
    (* Add mp.Instances as the default recipes *)
    match from_type_decl env loc p td with
    | Type -> assert false
    | Or t2s ->
        let default_instances =
          try
            let p = m#lookup_module "Instances" in
            [ Direct (`In, Typpx.Untypeast.lident_of_path p, Some p) ]
          with
          | Not_found -> []
        in
        `Ok (Or (t2s @ default_instances))
  with _ -> `Error (`No_imp_spec (loc, mp))

let from_module_path env imp_loc mp =
  let md =
    try Env.find_module mp env with _ ->
      eprintf "%a: BUG of ppx_implicits: Unbound module %a." Location.format imp_loc Path.format mp;
      assert false
  in
  from_module_type env md.md_loc mp md.md_type

(** spec to candidates *)

open Candidate
    
let rec cand_static env loc : t2 -> t list = function
  | Aggressive x ->
      map (fun x -> { x with aggressive = true }) & cand_static env loc x
  | Opened (f,x) -> cand_opened env loc (f,x)
  | Direct (f,x,popt) -> cand_direct env loc (f,x,popt)
  | Name (_, rex, t2) -> cand_name rex & fun () -> cand_static env loc t2
  | Typeclass (Some p) -> Ctypeclass.cand_typeclass env loc p
  | Typeclass None -> assert false
  | spec when is_static spec -> assert false
  | _ -> assert false

let rec cand_dynamic env loc ty = function
  | Related -> Crelated.cand_related env loc ty
  | Aggressive x -> map (fun x -> { x with aggressive= true }) & cand_dynamic env loc ty x
  | Name (_, rex, t2) -> cand_name rex & fun () -> cand_dynamic env loc ty t2
  | Deriving lid -> Cderiving.cand_deriving env loc ty lid
  | PPXDerive (_e,_cty,None) -> assert false
  | PPXDerive (_e,_cty,Some _ty) -> assert false
  | Opened _ | Direct _ | Typeclass _ ->
      (* they are static *)
      assert false

let candidates env loc = function
  | Type -> assert false (* This should not happen *)
  | Or ts ->
      let statics, dynamics = partition is_static ts in
      let statics = concat & map (cand_static env loc) statics in
      if !Options.debug_resolve then begin
        !!% "debug_resolve: static candidates@.";
        flip iter statics & fun x ->
          !!% "  %a@." Pprintast.expression (Typpx.Untypeast.untype_expression x.expr)
      end;
      let dynamics ty = concat & map (cand_dynamic env loc ty) dynamics in
      fun ty -> uniq & statics @ dynamics ty
    
