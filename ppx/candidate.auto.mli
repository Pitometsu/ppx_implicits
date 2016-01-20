type t = {
  lid : Longident.t;
  path : Path.t;
  expr : Typedtree.expression;
  type_ : Types.type_expr;
  aggressive : bool;
}
val uniq : t list -> t list
val scrape_sg :
  'a -> Env.t -> Types.module_declaration -> Ppxx.Compilerlib.Types.signature
val _test_scrape_sg : Path.t -> Env.t -> Types.signature_item list -> unit
val values_of_module :
  recursive:bool ->
  Env.t -> Longident.t -> Path.t -> Types.module_declaration -> t list
val get_opens : Env.t -> Path.t list
val _dump_summary : Env.t -> unit
val module_lids_in_open_path :
  Env.t -> Longident.t list -> Path.t option -> Path.t list
val data_types : Env.t -> Types.type_expr -> Path.t list
val related_modules : Env.t -> Types.type_expr -> Path.t list
val check_module : Env.t -> Location.t -> Path.t -> Types.module_declaration
val cand_direct :
  Env.t -> Location.t -> (Longident.t * Path.t option) Spec.flagged -> t list
val cand_related : Env.t -> 'a -> Types.type_expr -> t list
val cand_opened : Env.t -> Location.t -> Longident.t Spec.flagged -> t list
val cand_typeclass : Env.t -> Location.t -> Path.t -> t list
val cand_name : Re.re -> (unit -> t list) -> t list
