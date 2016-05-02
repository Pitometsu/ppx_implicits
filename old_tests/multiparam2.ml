(* multi parameter class with [@@typeclass] and [@@instance] *)

module type Num = sig
  type a
  type b
  type c
  val add : a -> b -> c
end [@@typeclass]

let add = Num.add

module M = struct
  module NumIntIntInt = struct
    type a = int
    type b = int
    type c = int
    let add = (+)
  end [@@instance Num]

  module NumFloatFloatFloat = struct
    type a = float
    type b = float
    type c = float
    let add = (+.)
  end [@@instance Num]

  module NumIntIntFloat = struct
    type a = int
    type b = int
    type c = float
    let add x y = float (x + y)
  end [@@instance Num]
end

open M
let () = assert (add 1 2 = 3)
let () = assert (add 1.2 3.4 = 4.6)
let () = assert (add 1 2 = 3.0)
