type ('a, 'spec) t = private 'a
(** [(ty, spec) t] is as same as [ty], but its value is determined by [spec].
*)

exception Not_resolved

val from_Some : 'a option -> 'a
(** raises [Not_resolved] when [None] is applied *)

external get : ('a, 'spec) t -> 'a = "%identity"

val imp : ?d:('a, 'spec) t -> 'a

external embed : 'a -> ('a, 'spec) t = "%identity"
