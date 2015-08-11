open Types

type t = 
  | Or of t2 list
  | Type (** [%imp].  No allowed in [%%imp_policy] *)

and t2

val to_string : t -> string
(** convert [t] to its string representation for [[%%imp_policy ...]] *)

val to_mangled_string : t -> string
(** convert [t] to its mangled string representation *)

val from_ok :
  Location.t ->
  [< `Error of
       [< `Failed_unmangle of string
        | `Parse of string
        | `ParseExp of 'a * string
        | `String of string ]
   | `Ok of 'b ] ->
  'b

val from_payload :
  Parsetree.payload ->
  [> `Error of
       [> `ParseExp of Parsetree.expression * string | `String of string ]
   | `Ok of t ]

val from_type_decl : Path.t -> Location.t -> type_declaration -> t
(** get policy from type __imp_policy__ = .. *)

val from_module_type : Path.t -> Location.t -> module_type -> t option
(** get policy from a module type which has type __imp_policy__ = .. *)

val from_module_path : Env.t -> Path.t -> t
(** get policy from a module path which has type __imp_policy__ = .. *)

val check_module_path_accessibility :
  Env.t ->
  Location.t ->
  Path.t ->
  [> `Accessible of Longident.t * module_declaration
   | `Not_found
   | `Shadowed ]

type result = Longident.t * Path.t * value_description * bool (* bool : aggressive *)

val uniq : result list -> result list

val candidates 
      : Env.t 
      -> Location.t 
      -> t 
      -> type_expr 
      -> result list
