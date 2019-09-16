open Types
open Asttypes

val is_klabel : arg_label -> [`Normal | `Optional] option
val extract : Env.t -> type_expr -> (arg_label * type_expr) list * type_expr
val extract_aggressively : Env.t -> type_expr -> ((arg_label * type_expr) list * type_expr) list
