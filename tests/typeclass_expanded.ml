module type Show  = sig
  type a 
  val show : a -> string
end

(* The following code can be auto-gened from the above definition of 
   [module type Show] *)
module Show = struct
  type 'a _module = (module Show with type a = 'a)

  type __class__
    
  type 'a _class = ('a _module, [%imp_spec has_type __class__]) Ppx_implicits.t

  let show (type a) ?_d:(_d : a _class option) =
    let module M = (val (Ppx_implicits.(get (from_Some _d)))) in
    M.show
end

let show = Show.show

module M = struct
  module ShowInt = struct
    type a = int
    let show = string_of_int
  end
    
  (* The following code can be auto-gened from the above definition of 
     [module ShowInt] with the name [Show] *)
  module ShowIntInstance = struct
    let dict: ShowInt.a Show._module = (module ShowInt)
    type __imp_instance_of__ = Show.__class__
  end

  module ShowFloat = struct
    type a = float
    let show = string_of_float
  end

  (* The following code can be auto-gened from the above definition of 
     [module ShowFloat] with the name [Show] *)
  module ShowFloatInstance = struct
    let dict: ShowFloat.a Show._module = (module ShowFloat)
    type __imp_instance_of__ = Show.__class__
  end
end

open M
let () = assert ((show 1) = "1")
let () = assert ((show 1.2) = "1.2")
