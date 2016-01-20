val data_types : Env.t -> Types.type_expr -> Path.t list
val related_modules : Env.t -> Types.type_expr -> Path.t list
val cand_related : Env.t -> 'a -> Types.type_expr -> Candidate.t list
