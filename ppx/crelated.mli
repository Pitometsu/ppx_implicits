open Types

val data_types : Env.t -> type_expr -> Path.t list
val related_modules : Env.t -> type_expr -> Path.t list
val cand_related : Env.t -> Location.t -> type_expr -> Candidate.t list
