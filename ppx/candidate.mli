open Types
open Typedtree

type t = {
  lid        : Longident.t; (** lid of candidate identifier. Used for [name] spec *)
  path       : Path.t;      (** path of candiidate identifier. Used for [uniq] *)
  expr       : expression;  (** candidate expression maker *)
  type_      : type_expr;
  aggressive : bool
}

val uniq : t list -> t list
(** Remove dupes *)

val cand_direct : Env.t -> Location.t -> ([`In | `Just] * Longident.t * Path.t option) -> t list
val cand_related : Env.t -> Location.t -> Types.type_expr -> t list
val cand_opened : Env.t -> Location.t -> ([`In | `Just] * Longident.t) -> t list
val cand_typeclass : Env.t -> Location.t -> Path.t -> t list
val cand_name : Re.re -> (unit -> t list) -> t list
  
