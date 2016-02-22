type t
(** Size of the type.

    It is implemented as a polynomial:
    
    * Each type variable node 'a is a variable a in the polynomial 
    * Each non type variable node is counted as a constant 1

    For example:

      ['a -> 'b] has size [a + b + 1].
      [(int * 'a)] has size [a + 2].

    The ordering of the type sizes are defined as follows:
    Let t and t' be type sizes.  t < t' iff

      S(t) < S(t') for any variable substitution S which maps
      all the variables in the polynomials s and s' to natural number (>=0).

    Therefore,

      ['a -> 'b -> int] < ['a -> 'b -> 'c -> float]
      but the following is not true: ['a -> 'a] < ['a -> 'b -> 'c]
*)

val to_string : t -> string
(** Print [t] *)

val size : Types.type_expr -> t
(** Obtain type size of the given type *)

val lt : t -> t -> bool
(** Type size comparison.  [lt t t'] returns true if  t < t' *)

val has_var : t -> bool
(** Returns [true] if the size has any variable *)

