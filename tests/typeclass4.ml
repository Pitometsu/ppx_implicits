module type Show = sig
  type a
  val show : a -> string
end [@@typeclass]

let show = Show.show

module Int = struct
  module ShowInt = struct
    type a = int
    let show  = string_of_int
  end

  (* The above with [%implicit Show] should produce the following *)
 
  module ShowInstance = struct
    let int : int Show.s = (module ShowInt)
  end
end

open Int
let () = assert (show 1 = "1")
