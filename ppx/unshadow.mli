open Typedtree

val check_module_path 
      : Env.t 
      -> Path.t 
      -> [ `Accessible of Longident.t (** Accessible via the returned Longident.t *)
         | `Not_found of Path.t (** Error. The path is not in env *)
         | `Shadowed of Ident.t * Ident.t * Path.t (** [`Shadowed (id, id', path)]: Ident [id] is shadowed. If [id] is aliased as [id'], then [id'] will be accessible as [path]. *)
         ]
(** [check_module_path env p] checks [p] is accessible in the environment [env]. 
    Here, "accessible" means the Longident [Untypeast.lident_of_path path] is
    resolved to [p] itself.
*)
  
val aliases : (Ident.t * Ident.t) list ref
(** Aliases required for unshadowing *)

val map_structure : (Ident.t * Ident.t) list -> structure -> structure
(** [map_strucure aliases str] unshadow shadowed module identifiers by aliasing them.

    If [str] has [module M = ...] and [M] is bound in the alias with [M'], then
    [map_structure aliases str] adds [module M' = M] just after the definition of [M].

    This only fixes the definition of [M]. The uses of [M] must be replaced by [M']
    must be done somewhere else, where [check_module_path] is used.
*)
  
