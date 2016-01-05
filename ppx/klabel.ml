(** Constraint labels *)
  
open Utils
open Ppxx.Compilerlib
open Types
open List

let is_klabel l =
  let len = String.length l in
  if len >= 2 && String.unsafe_get l 0 = '_' then Some `Normal
  else if len >= 3 && String.sub l 0 2 = "?_" then Some `Optional
  else None

(* Constraint labels must precede the other arguments *)
let rec extract env ty = 
  let ty = Ctype.expand_head env ty in
  match repr_desc ty with
  | Tarrow(l, ty1, ty2, _) when is_klabel l <> None ->
      let cs, ty = extract env ty2 in
      (l,ty1)::cs, ty
  | Tarrow(l, ty1, ty2, x) ->
      let cs, ty = extract env ty2 in
      cs, { (Ctype.newty & Tarrow (l, ty1, ty, x)) with level = ty.level }
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

    
