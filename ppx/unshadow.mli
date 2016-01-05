open Typedtree

val reset : unit -> unit
(** Reset replace ident pairs *)
  
module Replace : sig
  val replace : expression -> expression
  (** replace identifiers inaccessible in the typing environment due to
      shadowing by new identifier.  The old and new ident pair is recorded
      internally and used for [Alias.insert] to introduce the necessary
      module aliases *)
end
  
module Alias : sig  
  val insert : structure -> structure
  (** [insert aliases str] unshadow shadowed module identifiers by aliasing them.

      If [str] has [module M = ...] and [M] is bound in the alias with [M'], then
      [insert aliases str] adds [module M' = M] just after the definition of [M].
  
      This only fixes the definition of [M]. The uses of [M] must be replaced by [M']
      must be done somewhere else, where [check_module_path] is used.
  *)
end
