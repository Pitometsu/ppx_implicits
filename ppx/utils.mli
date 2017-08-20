module String : sig
  include module type of struct include Ppxx.Utils.String end

  val drop : int -> string -> string

  val is_prefix : ?from:int -> string -> string -> string option
  (** is_prefix "hello" "hello world" = Some " world" *)
end 

val scrape_sg
  : Path.t
  -> Env.t
  -> Types.module_declaration
  -> Types.signature

(** Build an empty type env except mp module with the given module type *)
class dummy_module
  : Env.t
  -> Path.t
  -> Types.module_type
  -> object
       method lookup_module : string -> Path.t
       method lookup_type   : string -> Path.t * Types.type_declaration
       method lookup_value  : string -> Path.t
     end

val exit_then : 'a -> (unit -> 'a) -> 'a
(** if Exit is raised, catch it and return the default *)

module Result : sig
  type ('a, 'err) t = ('a, 'err) result
  val at_Error : ('err -> 'a) -> ('a, 'err) t -> 'a
  module Monad : sig
    val (>>=) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  end
end

val at_Error : ('err -> 'a) -> ('a, 'err) Result.t -> 'a
(** Alias of [Result.at_Error] *)

module Option : sig
  include module type of struct include Ppxx.Utils.Option end
  exception Is_None

  val from_Some : 'a option -> 'a (** may raise Is_None *)

  module Monad : sig
    val (>>=) : 'a option -> ('a -> 'b option) -> 'b option
    val return : 'a -> 'a option
    val some : 'a -> 'a option
  end
end

val from_Some : 'a option -> 'a (** may raise Option.Is_None *)
  
module List : sig
  include module type of struct include Ppxx.Utils.List end

  val split_at : int -> 'a list -> 'a list * 'a list
  (** Haskell's [splitAt] *)
end

val mangle : string -> string
(** convert an arbitrary string to Lexer.identchar's
   '_' is a special char. 
*)

val unmangle : string -> (string, [> `Failed_unmangle of string ]) Result.t

val expression_from_string : string -> (Parsetree.expression, [> `Parse of string ]) Result.t

val tvars_of_core_type : Parsetree.core_type -> string list

(** [[%sig ..]] is not yet possible in 4.02.3. This is a workaround.

   [sig_of_stri [%stri module type X : S] ]
   ==> [module X : S ]
*)
val sig_module_of_stri : Parsetree.structure_item -> Parsetree.signature_item

val values_of_module
  : recursive:bool -> (*+ dig in the sub-modules or not *)
  Env.t ->
  Location.t -> (*+ Location of the environment obtained *) 
  Path.t ->
  Path.t list
(** Extract values defined in the specified Path.t *)

val format_expression : Format.formatter -> Typedtree.expression -> unit

val is_none : Typedtree.expression -> Types.type_expr option
