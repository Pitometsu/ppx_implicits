open Types
open Asttypes

val is_klabel : label -> [`Normal | `Optional] option
val extract : Env.t -> type_expr -> (label * type_expr) list * type_expr
val extract_aggressively : Env.t -> type_expr -> ((label * type_expr) list * type_expr) list
