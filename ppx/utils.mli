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

val mangle : string -> string
(** convert an arbitrary string to Lexer.identchar's
   '_' is a special char. 
*)

val unmangle : string -> [> `Ok of string | `Error of [> `Failed_unmangle of string ] ]

module Result : sig
  val (>>=) : [< `Error of 'a | `Ok of 'b ] -> ('b -> ([> `Error of 'a ] as 'c)) -> 'c
  val ok : 'a -> [> `Ok of 'a ]
  val return : 'a -> [> `Ok of 'a ]
  val error : 'a -> [> `Error of 'a ]
  val protect : (unit -> 'a) -> [> `Error of [> `Exn of exn ] | `Ok of 'a ]
  val map_error : ('a -> 'b) ->
    [< `Error of 'a | `Ok of 'c ] ->
    [> `Error of 'b | `Ok of 'c ]
  val mapM : ('a -> [< `Error of 'b | `Ok of 'c ]) ->
    'a list -> [ `Error of 'b | `Ok of 'c list ]
end

