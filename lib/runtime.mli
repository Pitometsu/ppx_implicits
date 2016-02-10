type ('a, 'spec) t = private 'a

exception Not_resolved

val imp : ('a, 'spec) t option -> 'a

val embed : 'a -> ('a, 'spec) t option
