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

val (>>=) : [< `Error of 'a | `Ok of 'b ] -> ('b -> ([> `Error of 'a ] as 'c)) -> 'c
