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
