module type Show = sig
  type a
  val show : a -> string
end [@@typeclass]

let show = Show.show

module Int = struct
  module M = struct
    type a = int
    let show  = string_of_int
  end [@@instance Show]
end

module Float = struct
  module M = struct
    type a = float
    let show  = string_of_float
  end [@@instance Show]
end

open Int
open Float

let () = assert (show 1 = "1")
let () = assert (show 1.2 = "1.2")
