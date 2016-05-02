module type Num  = sig
  type a type b type c
  val add : a -> b -> c
end

module Num = struct
  type ('a,'b,'c) _module = (module Num with type a = 'a and type b = 'b and type c = 'c)

  type __class__

  type ('a,'b,'c) _class =
      (('a,'b,'c) _module,
       [`Spec_has__type_20_28____class_____29 of < l0 :__class__  > ])
        Ppx_implicits.t
  
  let add (type a) (type b) (type c)
      ?_d:((_d : (a,b,c) _class option))  =
    let module M = (val
                    let open Ppx_implicits in get (from_Some _d)) in M.add
end

let add = Num.add

module IntIntInt = struct
  module NumIntIntInt = struct
    type a = int
    type b = int
    type c = int
    let add = (+)
  end

  module NumIntIntIntInstance = struct
    let dict : (NumIntIntInt.a,NumIntIntInt.b,NumIntIntInt.c) Num._module
        = (module NumIntIntInt)
    type __imp_instance_of__ = Num.__class__
  end
end

module FloatFloatFloat = struct
  module NumFloatFloatFloat = struct
    type a = float
    type b = float
    type c = float
    let add = (+.)
  end

  module NumFloatFloatFloatInstance = struct
    let dict: (NumFloatFloatFloat.a,NumFloatFloatFloat.b,NumFloatFloatFloat.c) Num._module
        = (module NumFloatFloatFloat)
    type __imp_instance_of__ = Num.__class__
  end
end

module IntIntFloat = struct
  module NumIntIntFloat = struct
    type a = int
    type b = int
    type c = float
    let add x y = float (x + y)
  end

  module NumIntIntFloatInstance = struct
    let dict:
        (NumIntIntFloat.a,NumIntIntFloat.b,NumIntIntFloat.c) Num._module =
      (module NumIntIntFloat)
    type __imp_instance_of__ = Num.__class__
  end
end

open IntIntInt
open FloatFloatFloat
open IntIntFloat

let () = assert (add 1 2 = 3)
let () = assert (add 1.2 3.4 = 4.6)
let () = assert (add 1 2 = 3.0)
