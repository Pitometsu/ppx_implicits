module type Show = sig
  (* Parameters must be properly listed. 
     We cannot add parameters using include S *)
  type a 
  val show : a -> string
end [@@typeclass]

let show = Show.show

module M = struct
  module ShowInt = struct
    type a = int
    let show  = string_of_int
  end [@@instance Show]

  module ShowFloat = struct
    type a = float
    let show  = string_of_float
  end [@@instance Show]
end

open M
let () = assert (show 1 = "1")
let () = assert (show 1.2 = "1.2")

let show_aliased = show
let () = assert (show_aliased 3.4 = "3.4")

let show_twice ?_d x = show ?_d x ^ show ?_d x
let () = assert (show_twice 5.6 = "5.65.6")

let show_twice' : ?_d:'a Show._class -> 'a -> string = fun ?_d x -> show x ^ show x
let () = assert (show_twice' 5.6 = "5.65.6")

let show_twice'' ?_d:(imp:'a Show._class option) (x : 'a) = show x ^ show x
let () = assert (show_twice'' 5.6 = "5.65.6")

(* This does not work: imp and x have no relationship.
let show_twice'' ?_d:(imp:'a Show._class option) x = show x ^ show x
let () = assert (show_twice'' 5.6 = "5.65.6")
*)
  
(* This does not work either.
let show_twiceX = fun ?_d x -> show x ^ show x
let () = assert (show_twiceX 5.6 = "5.65.6")
*)
