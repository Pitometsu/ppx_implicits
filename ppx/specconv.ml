open Ppxx.Utils
open List
open Format

open Ppxx.Compilerlib

open Parsetree
open Types

open Spec

(* Spec conversion 

   * between attribute expression: [%imp <e>] <=> spec
   * between type definition: type __imp_spec__ <=> [%%imp_spec <e>] <=> spec
*)

let prefix = "Spec_"
let prefix_len = String.length prefix

let get_type_components = 
  let rec t = function
    | Type -> []
    | Or xs -> concat_map t2 xs
  and t2 = function
    | Direct (`In, _l, _) -> []
    | Direct (`Just, _l, _) -> []
    | Opened (`In, _l) -> []
    | Opened (`Just, _l) -> []
    | Related -> []
    | Aggressive x -> t2 x
    | Name (_s, _re, x) -> t2 x
    | Typeclass _ -> []
    | Deriving _p -> []
    | PPXDerive (_e, cty, _) -> [cty]
  in
  t 

let assign_type_components tys t0 = 
  let rec t tys = function
    | Type -> tys, Type
    | Or xs ->
        let tys, rev_xs = 
          fold_left (fun (tys,rev_xs) x ->
            let tys, x = t2 tys x in
            tys, x :: rev_xs) (tys,[]) xs
        in
        tys, Or (rev rev_xs)
          
  and t2 tys x = match x with
    | Direct _ -> tys, x
    | Opened _ -> tys, x
    | Related -> tys, x
    | Aggressive x ->
        let tys, x = t2 tys x in
        tys, Aggressive x
    | Name (s, re, x) ->
        let tys, x = t2 tys x in
        tys, Name (s, re, x)
    | Typeclass _ -> tys, x
    | Deriving _ -> tys, x
    | PPXDerive (e, cty, None) ->
        begin match tys with
        | ty::tys -> tys, PPXDerive (e, cty, Some ty)
        | _ -> assert false
        end
    | PPXDerive (_e, _cty, Some _) -> assert false
  in
  match t tys t0 with
  | [], t0 -> t0
  | _ -> assert false

let mangle x =
  (prefix ^ Utils.mangle (to_string x),
   get_type_components x)

(* CR jfuruse: need tests *)
let unmangle_spec_string s = 
  if not & String.is_prefix prefix s then assert false; (* CR jfuruse: better error *)
  let s = String.sub s prefix_len (String.length s - prefix_len) in
  Utils.unmangle s

let from_string = Utils.expression_from_string

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
              begin match e.pexp_desc with
              | Pexp_constraint (e, cty) -> PPXDerive (e, cty, None)
              | _ -> errorf "ppxderive must take (e : t)"
              end
          | _ -> errorf "ppxderive must take just one argument"
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
  | { type_params = _
    ; type_kind = Type_variant [ { cd_id= id; cd_args = tys; cd_res = None; cd_loc = loc} ]
    ; type_manifest = None } ->
      let open Result.Monad in
      fix_typeclass loc p
      & assign_type_components tys
      & from_Ok (error loc)
      & unmangle_spec_string id.Ident.name
        >>= from_string
        >>= from_expression env
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

