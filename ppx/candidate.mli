open Types
open Typedtree

type t = {
  path       : Path.t;
  (** path of candiidate identifier. Used for [name] spec, [Candidate.uniq] and recursive call size check *)

  expr       : expression;
  (** candidate expression. Candidate can be not only a simple Path.t but also an expression using Path.t *)

  type_      : type_expr;
  (** The type of the candidate *)

  aggressive : bool
}

val format : Format.formatter -> t -> unit

val uniq : t list -> t list
(** Remove dupes. Identification of [Candidate.t] is done by [path] fields *)

val default_candidate_of_path : Env.t -> Path.t -> t
(** Build a default candidate: 
    * expr is just the path
    * type_ is the type of path
    * aggressive= false 
*)

val cand_direct : Env.t -> Location.t -> ([`In | `Just] * Longident.t * Path.t option) -> t list
val cand_opened : Env.t -> Location.t -> ([`In | `Just] * Longident.t) -> t list
val cand_name : Re.re -> (unit -> t list) -> t list
