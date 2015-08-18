(* multi parameter class *)

module type Num = sig
  type a
  type b
  type c
  val add : a -> b -> c
end

module Num = struct

  type ('a, 'b, 'c) s = (module Num with type a = 'a and type b = 'b and type c = 'c)
  type ('a, 'b, 'c) t = Packed of ('a, 'b, 'c) s
  (* t is still required since s is just an alias of (module ..) *)

  module Instances = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end

  [@@@warning "-16"] (* required for unpack_opt *)

  let unpack_opt ?_imp = match _imp with
    | None -> assert false
    | Some (Packed x) -> x

  [%%imp_spec opened NumInstance]

  let add (type a) (type b) (type c) ?_imp:(_imp:(a, b, c) t option) = 
    let module M = (val (unpack_opt ?_imp)) in 
    M.add
end

let add = Num.add

module IntIntInt = struct
  module NumIntIntInt = struct
    type a = int
    type b = int
    type c = int
    let add = (+)
  end

  module NumInstance = struct
    let intintint : (int, int, int) Num.s = (module NumIntIntInt)
  end
end

module FloatFloatFloat = struct
  module NumFloatFloatFloat = struct
    type a = float
    type b = float
    type c = float
    let add = (+.)
  end

  module NumInstance = struct
    let floatfloatfloat : (float, float, float) Num.s = (module NumFloatFloatFloat)
  end
end

module IntIntFloat = struct
  module NumIntIntFloat = struct
    type a = int
    type b = int
    type c = float
    let add x y = float (x + y)
  end

  module NumInstance = struct
    let floatfloatfloat : (int, int, float) Num.s = (module NumIntIntFloat)
  end
end

open IntIntInt
open FloatFloatFloat
open IntIntFloat
let () = assert (add 1 2 = 3)
let () = assert (add 1.2 3.4 = 4.6)
let () = assert (add 1 2 = 3.0)
