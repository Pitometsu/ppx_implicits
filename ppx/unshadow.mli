open Typedtree

val reset : unit -> unit
(** Reset replace ident pairs *)
  
module Replace : sig
  val replace : expression -> expression
  (** Replace identifiers inaccessible in the typing environment due to
      shadowing by new identifier.  The old and new ident pair is recorded
      internally and used for [Alias.insert] to introduce the necessary
      module aliases.

      Example: 

        Environment: 
          module M__1 = struct let x = 1 end
          module M__2 = ...

        Expression:
          M__1.x + 1   (* M__1.x is inaccessible since M__2 shadows the name M *)

        The expression is replaced to:
          M__fresh.x + 1

        and the pair (M__1, M__fresh) is recorded.
   *)
end
  
module Alias : sig  
  val insert : structure -> structure
  (** [insert str] unshadow shadowed module identifiers by aliasing them.

      If [str] has [module M = ...] and [M] is bound in the alias with [M'], then
      [insert aliases str] adds [module M' = M] just after the definition of [M].
  
      This only fixes the definition of [M]. The uses of [M] must be replaced by [M']
      must be done somewhere else, where [check_module_path] is used.

      Example: following the above one:

        module M__1 = ...
        module M__fresh = M_1
        module M__2 = ...
  *)
end
