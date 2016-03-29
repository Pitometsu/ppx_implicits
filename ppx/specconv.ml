open Ppxx.Utils
open List

open Ppxx.Compilerlib
open Typpx.Compilerlib

open Parsetree
open Types

open Spec

(* Spec conversion 

   * between attribute expression: [%imp <e>] <=> spec
   * between type definition: type __imp_spec__ <=> [%%imp_spec <e>] <=> spec
*)

let prefix = "Spec_"
let prefix_len = String.length prefix

(** Extract type parts so that they can be encoded in the parameter part
    of a variant constructor.
*)
let get_type_components = 
  let rec t = function
    | xs -> concat_map t2 xs
  and t2 = function
    | Direct (`In, _l, _) -> []
    | Direct (`Just, _l, _) -> []
    | Opened (`In, _l) -> []
    | Opened (`Just, _l) -> []
    | Related -> []
    | Aggressive x -> t2 x
    | Name (_s, _re, x) -> t2 x
    | Has_type (cty, _ty) -> [cty]
    | Deriving _p -> []
    | PPXDerive (_e, cty, _) -> [cty]
  in
  t 

(** Assign types of variant constructor parameter to spec *)
let assign_type_components tys t0 = 
  let rec t tys = function
    | xs ->
        let tys, rev_xs = 
          fold_left (fun (tys,rev_xs) x ->
            let tys, x = t2 tys x in
            tys, x :: rev_xs) (tys,[]) xs
        in
        tys, (rev rev_xs)
          
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
    | Has_type (cty, None) ->
        begin match tys with
        | ty::tys -> tys, Has_type (cty, Some ty)
        | _ -> assert false (* impos *)
        end
    | Has_type _ -> assert false (* impos *)
    | Deriving _ -> tys, x
    | PPXDerive (e, cty, None) ->
        begin match tys with
        | ty::tys -> tys, PPXDerive (e, cty, Some ty)
        | _ -> assert false (* impos *)
        end
    | PPXDerive (_e, _cty, Some _) -> assert false (* impos *)
  in
  match t tys t0 with
  | [], t0 -> t0
  | _ -> assert false (* impos *)

let mangle x =
  (prefix ^ Utils.mangle (to_string x),
   get_type_components x)

(* CR jfuruse: need tests *)
let unmangle_spec_string s = 
  if not & String.is_prefix prefix s then errorf "Mangled spec string does not start with \"Spec_\": %s" s;
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
      | Pexp_tuple xs -> map t2 xs
      | _ -> [t2 e]
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
      | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "has_type"} }, args ) ->
           begin match args with
          | ["", e] ->
              begin match e.pexp_desc with
              | Pexp_ident lid ->
                  Has_type (Ppxx.Helper.Typ.constr lid [], None)
              | _ -> errorf "has_type must take a type path"
              end
          | _ -> errorf "has_type must take just one argument"
          end
          
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
          errorf "%a: Illegal spec expression: %a" 
            Location.format e.pexp_loc
            Pprintast.expression e
            
    in
    `Ok (t e)
  with
  | Failure s -> `Error (`ParseExp (e, s))

let from_structure env str =
  match str with
  | [] -> `Error (`String "requires implicit policies")
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

(********************* NEW ENCODING USING POLYVARIANT ***************)
    
(** Obtain a spec from a type expr. *)
let from_type_expr env loc ty = match expand_repr_desc env ty with
  | Tvariant rd ->
      let rd = Btype.row_repr rd in
      begin match rd with
      | { row_closed= true; row_fields= [l, Rpresent (Some ty)] } ->
          let open Utils in
          let open Utils.Result.Monad in
          (* Note that the type variables are Tunivars *)
          let unpoly ty = match expand_repr_desc env ty with
            | Tpoly (ty, []) -> ty
            | _ -> ty
          in
          let fs, nil = Ctype.(flatten_fields & object_fields ty) in
          let fs = map (fun (a,b,ty) -> (a,b,unpoly ty)) fs in
          assert (expand_repr_desc env nil = Tnil);
          assert (fs = []
                 || for_all (function (_, Fpresent, _) -> true | _ -> false) fs);
          assign_type_components (map (fun (_,_,ty) -> ty) fs)
          & from_Ok (error loc)
          & unmangle_spec_string l
            >>= from_string
            >>= from_expression env
      | _ -> 
          errorf "%a: Illegal type for implicit spec" Location.format loc
      end
  | _ -> 
      errorf "%a: Illegal type for implicit spec" Location.format loc

let to_core_type _loc spec =
  let open Ppxx.Helper in
  let mangled, ctys = mangle spec in
  let label n = "l" ^ string_of_int n in
  let make_meth_type cty =  (* quantify cty *)
    Typ.poly (Utils.tvars_of_core_type cty) cty
  in
  let oty = Typ.object_ (mapi (fun n cty -> (label n, [], make_meth_type cty)) ctys) Closed
  in
  Typ.variant [ Parsetree.Rtag (mangled, [], false, [oty]) ] Closed None
