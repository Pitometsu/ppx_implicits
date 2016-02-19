type ('a, 'spec) t = private 'a

type ('a, 'spec) s = ?_d: ('a, 'spec) t -> 'a
  
exception Not_resolved

val from_Some : 'a option -> 'a
(** raises [Not_resolved] when [None] is applied *)

external get : ('a, 'spec) t -> 'a = "%identity"

val imp : ?_d:('a, 'spec) t -> 'a

val embed : 'a -> ('a, 'spec) t option
