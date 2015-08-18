open Types

type t = 
  | Or of t2 list
  | Type (** [%imp].  No allowed in [%%imp_spec] *)

and t2

val to_string : t -> string
(** convert [t] to its string representation for [[%%imp_spec ...]] *)

val to_mangled_string : t -> string
(** convert [t] to its mangled string representation *)

val error :
  Location.t
  -> [< `Failed_unmangle of string
     | `Parse of string
     | `ParseExp of 'a * string
     | `String of string ]
  -> 'fail

val from_payload :
  Parsetree.payload ->
  [> `Error of
       [> `ParseExp of Parsetree.expression * string | `String of string ]
   | `Ok of t ]

val from_type_decl : Path.t -> Location.t -> type_declaration -> t
(** get spec from type __imp_spec__ = .. *)

val from_module_type : Env.t -> Path.t -> Location.t -> module_type -> [> `Ok of t | `Error of [> `No_imp_spec of Location.t * Path.t ] ]
(** get spec from a module type which has type __imp_spec__ = .. *)

val from_module_path : imp_loc: Location.t -> Env.t -> Path.t -> [> `Ok of t | `Error of [> `No_imp_spec of Location.t * Path.t ] ]
(** get spec from a module path which has type __imp_spec__ = .. *)

type result = Longident.t * Path.t * value_description * bool (* bool : aggressive *)

val uniq : result list -> result list

val candidates 
      : Env.t 
      -> Location.t 
      -> t 
      -> type_expr 
      -> result list
