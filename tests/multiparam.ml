(* multi parameter class *)

module type Num = sig
  type a
  type b
  type c
  val add : a -> b -> c
end [@@typeclass]

let add = Num.add

module IntIntInt = struct
  module NumIntIntInt = struct
    type a = int
    type b = int
    type c = int
    let add = (+)
  end [@@instance Num]
end

module FloatFloatFloat = struct
  module NumFloatFloatFloat = struct
    type a = float
    type b = float
    type c = float
    let add = (+.)
  end [@@instance Num]
end

module IntIntFloat = struct
  module NumIntIntFloat = struct
    type a = int
    type b = int
    type c = float
    let add x y = float (x + y)
  end [@@instance Num]
end

open IntIntInt
open FloatFloatFloat
open IntIntFloat
let () = assert (add 1 2 = 3)
let () = assert (add 1.2 3.4 = 4.6)
let () = assert (add 1 2 = 3.0)
