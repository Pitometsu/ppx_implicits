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
    let add = (+)
  end [@@instance: (module Num with type a = int and type b = int and type c = int)]
end

module FloatFloatFloat = struct
  module NumFloatFloatFloat = struct
    let add = (+.)
  end [@@instance: (module Num with type a = float and type b = float and type c = float)]
end

module IntIntFloat = struct
  module NumIntIntFloat = struct
    let add x y = float (x + y)
  end [@@instance: (module Num with type a = int and type b = int and type c = float)]
end

open IntIntInt
open FloatFloatFloat
open IntIntFloat
let () = assert (add 1 2 = 3)
let () = assert (add 1.2 3.4 = 4.6)
let () = assert (add 1 2 = 3.0)
