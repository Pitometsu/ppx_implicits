(*

  Instance search space specification DSL, magling to and back from
  OCaml type definitions.

*)
open Ppxx.Utils
open List

open Ppxx.Compilerlib
open Typpx.Compilerlib
open Parsetree
open Types

module Forge = Typpx.Forge
  
(** spec dsl *)
type t = t2 list (** [t2, .., t2] *)

and t2 = 
  | Opened of [`In | `Just] * Longident.t (** [opened M]. The values defined under module path [P.M] which is accessible as [M] by [open P] *)
  | Direct of [`In | `Just] * Longident.t * Path.t option
    (** [P] or [just P]. [P] is for the values defined under module [P] and [P]'s sub-modules. [just P] is for values defined just under module [P] and values defined in its sub-modules are not considered. *)
  | Aggressive of t2 (** [aggressive t2]. Even normal function arrows are considered as constraints. *)
  | Related (** [related]. The values defined under module [P] where data type defined in [P] appears in the type of the resolution target *)
  | Name of string * Re.re * t2 (** [name "rex" t2]. Constraint values only to those whose names match with the regular expression *)
  | Has_type of core_type * type_expr option (** [typeclass path]. Typeclass style resolution.  *) 
  | Deriving of Longident.t (** [deriving M]. [M] must define [M.tuple], [M.object_] and [M.poly_variant] *)
  | PPXDerive of Parsetree.expression * core_type * type_expr option (** [ppxderive ([%...] : ty)]. *)
      
let rec is_static = function
  | Opened _ -> true
  | Direct _ -> true
  | Related -> false
  | Aggressive t2 | Name (_, _, t2) -> is_static t2
  | Has_type _ -> true
  | Deriving _ -> false
  | PPXDerive _ -> false
    
let to_string = 
  let rec t = function
    | [x] -> t2 x
    | xs -> String.concat ", " (map t2 xs)
  and t2 = function
    | Direct (`In, l, _) -> Longident.to_string l
    | Direct (`Just, l, _) -> Printf.sprintf "just %s" & Longident.to_string l
    | Opened (`In, l) -> Printf.sprintf "opened (%s)" & Longident.to_string l
    | Opened (`Just, l) -> Printf.sprintf "opened (just %s)" & Longident.to_string l
    | Related -> "related"
    | Aggressive x -> Printf.sprintf "aggressive (%s)" (t2 x)
    | Name (s, _re, x) -> Printf.sprintf "name %S (%s)" s (t2 x)
    | Has_type (cty, _) -> 
        Format.asprintf "has_type (%a)"
          Pprintast.core_type cty
    | Deriving p -> Printf.sprintf "deriving %s" & Longident.to_string p
    | PPXDerive (e, cty, _) ->
        Format.asprintf "ppxderive (%a : %a)"
          Pprintast.expression e
          Pprintast.core_type cty
  in
  t 

(** spec to candidates *)

open Candidate
    
let rec cand_static env loc : t2 -> t list = function
  | Aggressive x ->
      map (fun x -> { x with aggressive = true }) & cand_static env loc x
  | Opened (f,x) -> cand_opened env loc (f,x)
  | Direct (f,x,popt) -> cand_direct env loc (f,x,popt)
  | Name (_, rex, t2) -> cand_name rex & fun () -> cand_static env loc t2
  | Has_type (_, Some ty) -> Chas_type.cand_has_type env loc ty
  | Has_type _ -> assert false (* impos *)
  | spec when is_static spec -> assert false (* impos *)
  | _ -> assert false (* impos *)

let rec cand_dynamic env loc ty = function
  | Related -> Crelated.cand_related env loc ty
  | Aggressive x -> map (fun x -> { x with aggressive= true }) & cand_dynamic env loc ty x
  | Name (_, rex, t2) -> cand_name rex & fun () -> cand_dynamic env loc ty t2
  | Deriving lid -> Cderiving.cand_deriving env loc ty lid
  | PPXDerive (_e, _cty, None) -> assert false (* impos *)
  | PPXDerive (e, _cty, Some temp_ty) -> Cppxderive.cand_derive env loc e temp_ty ty 
  | Opened _ | Direct _ | Has_type _ ->
      (* they are static *)
      assert false (* impos *)

let candidates env loc = function
  | ts ->
      let statics, dynamics = partition is_static ts in
      let statics = concat & map (cand_static env loc) statics in
      if !Options.debug_resolve then begin
        !!% "debug_resolve: static candidates@.";
        flip iter statics & fun x ->
          !!% "  %a@." Pprintast.expression ((* Typpx.Untypeast.untype_expression *) (Untypeast.(default_mapper.expr default_mapper)) x.expr)
      end;
      let dynamics ty = concat & map (cand_dynamic env loc ty) dynamics in
      fun ty -> uniq & statics @ dynamics ty
    
