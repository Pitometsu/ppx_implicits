module type Show = sig
  (* Since ppx_implicits is a PPX pre-processor, parameters and methods
     must be properly coded in the module type declaration. [include] does
     not work. *)

  (* class parameters *)
  type a 

  (* methods *)
  val show : a -> string
end [@@typeclass] (* [@@typeclass] produces [module Show] just after this module type declaration. *)

let show = Show.show

module M = struct

  (* Instance declaration *)    
  module ShowInt = struct
    (* Parameter instantiations must not be declared here like [type a = int].
       They must be declared in [@@instance]. *)
    let show  = string_of_int
  end [@@instance: (module Show with type a = int)]
  (* [@@instance: (module X with type p1 = t1 and .. and type pn = tn)]
         
     The syntax is very redundant but OCaml 4.02.3 has no way to write
     module types directly in attribute payloads.

     This produces a module [XInstance] (here [ShowIntInstance]) with
     an appropriate implicit value definition.
  *)

  module ShowFloat = struct
    let show  = string_of_float
  end [@@instance: (module Show with type a = float)]
end

open M (* This makes the instances of Show available for [show] function *)

let () = assert (show 1 = "1")
let () = assert (show 1.2 = "1.2")

(* See implicit polymorphism works *)

let show_aliased = show
let () = assert (show_aliased 3.4 = "3.4")

(* Currently the implicit argument label is "_d". By explicit dispatching *)
let show_twice ?_d x = show ?_d x ^ show ?_d x
let () = assert (show_twice 5.6 = "5.65.6")

(* By dispatching by type *)
let show_twice' : ?_d:'a Show._class -> 'a -> string = fun ?_d x -> show x ^ show x
let () = assert (show_twice' 5.6 = "5.65.6")

(* By dispatching by type, another way *)
let show_twice'' ?_d:(_:'a Show._class option) (x : 'a) = show x ^ show x
let () = assert (show_twice'' 5.6 = "5.65.6")

(* This does not work: imp and x have no relationship.
let show_twice'' ?_d:(imp:'a Show._class option) x = show x ^ show x
let () = assert (show_twice'' 5.6 = "5.65.6")
*)
  
(* This does not work either.
let show_twiceX = fun ?_d x -> show x ^ show x
let () = assert (show_twiceX 5.6 = "5.65.6")
*)
