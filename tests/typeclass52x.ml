module type Show  = sig
  type a 
  val show : a -> string
end

module Show = struct
  [@@@warning "-16"]
  type 'a _module = (module Show with type a = 'a)

  type __here__
    
  type 'a _class = ('a _module, [%imp_spec typeclass __here__]) Ppx_implicits.Runtime.t

  let show (type a) ?_d:(_d : a _class option) =
    let module M = (val (Ppx_implicits.Runtime.(get (from_Some _d)))) in
    M.show
end

let show = Show.show

module M = struct
  module ShowInt = struct
    type a = int
    let show = string_of_int
  end
    
  module ShowIntInstance = struct
    let dict: ShowInt.a Show._module = (module ShowInt)

    type __instance_of__ = Show.__here__
  end

  module ShowFloat = struct
    type a = float
    let show = string_of_float
  end

  module ShowFloatInstance = struct
    let dict: ShowFloat.a Show._module = (module ShowFloat)

    type __instance_of__ = Show.__here__
  end
end

open M
let () = assert ((show 1) = "1")
let () = assert ((show 1.2) = "1.2")
