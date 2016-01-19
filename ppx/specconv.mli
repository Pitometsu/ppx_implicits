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

val from_type_decl : Env.t -> Location.t -> Path.t -> Types.type_declaration -> Spec.t
(** get spec from type __imp_spec__ = .. *)

val from_module_type :
  Env.t -> Location.t -> Path.t -> module_type
  -> (Spec.t, [> `No_imp_spec of Location.t * Path.t ]) Result.t

(** get spec from a module type which has type __imp_spec__ = .. *)

val from_module_path :
  Env.t -> Location.t -> Path.t
  -> (Spec.t, [> `No_imp_spec of Location.t * Path.t ]) Result.t
