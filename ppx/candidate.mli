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

val candidates 
  : Env.t 
    -> Location.t 
    -> Spec.t 
    -> type_expr 
    -> t list
