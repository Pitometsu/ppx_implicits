module type Show = sig
  type a
  val show : a -> string
end [@@typeclass]

let show = Show.show

module M = struct

  module ShowInt = struct
    type a = int
    let show  = string_of_int
  end [@@instance Show]

end

module N = struct
  module ShowFloat = struct
    type a = float
    let show  = string_of_float
  end [@@instance Show]

end

open M
open N

let () = assert (show 1 = "1")
let () = assert (show 1.2 = "1.2")
