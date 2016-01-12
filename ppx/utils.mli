val scrape_sg
  : Path.t
  -> Env.t
  -> Types.module_declaration
  -> Types.signature

(** Build an empty type env except mp module with the given module type *)
class dummy_module
  : Env.t
  -> Path.t
  -> Types.module_type
  -> object
       method lookup_module : string -> Path.t
       method lookup_type   : string -> Path.t * Types.type_declaration
       method lookup_value  : string -> Path.t
     end
