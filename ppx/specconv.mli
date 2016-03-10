open Utils
open Types

val mangle : Spec.t -> string * Parsetree.core_type list
(** convert [t] to its mangled string representation and type components *)

val error
  : Location.t
  -> [< `Failed_unmangle of string
     | `Parse of string
     | `ParseExp of 'a * string
     | `String of string ]
  -> 'fail

val from_payload
  : Env.t
  -> Parsetree.payload
  -> (Spec.t, [> `ParseExp of Parsetree.expression * string
              |  `String of string ]) Result.t

val from_type_expr :
  Env.t ->
  Location.t ->
  type_expr ->
  Spec.t

val to_core_type : Location.t -> Spec.t -> Parsetree.core_type
