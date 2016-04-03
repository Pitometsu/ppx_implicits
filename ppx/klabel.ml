(** Constraint labels *)
  
open Ppxx.Utils
open Typpx.Compilerlib
open Types
open List
open Asttypes

let is_klabel = function
  | Labelled s when s.[0] = '_' -> Some `Normal
  | Optional s when s.[0] = '_' -> Some `Optional
  | _ -> None

(* Constraint labels must precede the other arguments *)
let rec extract env ty = 
  let ty = Ctype.expand_head env ty in
  match repr_desc ty with
  | Tarrow(l, ty1, ty2, _) when is_klabel l <> None ->
      let cs, ty = extract env ty2 in
      (l,ty1)::cs, ty
(*
  | Tarrow(l, ty1, ty2, x) ->
      let cs, ty = extract env ty2 in
      cs, { (Ctype.newty & Tarrow (l, ty1, ty, x)) with level = ty.level }
*)
  | _ -> [], ty

let rec extract_aggressively env ty =
  let ty = Ctype.expand_head env ty in
  match repr_desc ty with
  | Tarrow(l, ty1, ty2, _) when gen_vars ty1 <> [] ->
      ([], ty)
      :: map
        (fun (cs, ty) -> (l,ty1)::cs, ty)
        (extract_aggressively env ty2)
  | _ -> [[], ty]

(*
let rec get_args ty =
  let ty = Ctype.expand_head env ty in
  match repr_desc ty with
  | Tarrow(l, ty1, ty2, _) -> (l,ty1)::get_args ty2
  | _ -> []
*)

