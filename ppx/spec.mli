open Types

(** spec dsl *)
type t = 
  | Or of t2 list 
    (** [t2, .., t2] *)

  | Type 
    (** [%imp].  Encoded as a type definition.  No allowed in [%%imp_spec] *)

and t2 = 
  | Opened of [`In | `Just] * Longident.t
    (** [opened M]

        The values defined under module path [P.M] which is accessible as [M] 
        by [open P] 
    *)
      
  | Direct of [`In | `Just] * Longident.t * Path.t option
    (** [P] or [just P]. 

        [P] is for the values defined under module [P] and [P]'s sub-modules. 
        [just P] is for values defined just under module [P] and values defined 
        in its sub-modules are not considered. 
    *)
      
  | Aggressive of t2 
    (** [aggressive t2]. 

        Even normal function arrows are considered as constraints. 
    *)
      
  | Related 
    (** [related]. 

        The values defined under module [P] where data type defined in [P] appears 
        in the type of the resolution target 
    *)
      
  | Name of string * Re.re * t2
    (** [name "rex" t2]. 

        Constraint values only to those whose names match with the regular expression 
    *)

  | Typeclass of Path.t option
    (** [typeclass]. 

        Typeclass style resolution. The argument is None at parsing, but must be 
        filled with Some until the resolution.

        None at parsing, but must be filled with Some until the resolution 
    *)
      
  | Deriving of Longident.t
    (** [deriving M]. 

        [M] must define [M.tuple], [M.object_] and [M.poly_variant] 
    *)

  | PPXDerive of Parsetree.expression * Parsetree.core_type * type_expr option (** [ppxderive ([%...] : ty)]. *)

val is_static : t2 -> bool
(** static : instance space is fixed
    dynamic : instance space can be changed according to the target type
*)

val to_string : t -> string
(** convert [t] to its string representation for [[%%imp_spec ...]] *)

val expression_from_string : string -> [> `Ok of Parsetree.expression | `Error of [> `Parse of string ]]
  
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
  Env.t
  -> Parsetree.payload 
  -> [> `Ok of t
     |  `Error of [> `ParseExp of Parsetree.expression * string 
                  |  `String of string ]
     ]

val from_type_decl : Env.t -> Location.t -> Path.t -> Types.type_declaration -> t
(** get spec from type __imp_spec__ = .. *)

val from_module_type : 
  Env.t -> Location.t -> Path.t -> module_type 
  -> [> `Ok of t 
     |  `Error of [> `No_imp_spec of Location.t * Path.t ] 
     ]
(** get spec from a module type which has type __imp_spec__ = .. *)

val from_module_path : 
  Env.t -> Location.t -> Path.t 
  -> [> `Ok of t 
     |  `Error of [> `No_imp_spec of Location.t * Path.t ] 
     ]
(** get spec from a module path which has type __imp_spec__ = .. *)

val candidates 
  : Env.t 
    -> Location.t 
    -> t 
    -> type_expr 
    -> Candidate.t list
