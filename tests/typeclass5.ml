module type Show = sig
  (* Parameters must be properly listed. We cannot add parameters using include S *)
  type a 
  val show : a -> string
end

(* The above with [%typeclass] should produce the following *)

module Show = struct

  type 'a s = (module Show with type a = 'a)

  include Ppx_implicits.Implicits.Make(struct
    type 'a t = 'a s
  end)

  [%%imp_policy opened2]

  let show (type a) ?_imp = let module M = (val (unpack_opt ?_imp : a s)) in M.show
end

let show = Show.show

module M = struct
  module ShowInt = struct
    type a = int
    let show  = string_of_int
  end

  (* The above with [%implicit Show] should produce the following. *)
 
  module ShowIntIntance = struct
    let int : ShowInt.a Show.s = (module ShowInt)
    type __imp_instance__ = Show.__imp_policy__
  end

  module ShowFloat = struct
    type a = float
    let show  = string_of_float
  end

  (* The above with [%implicit Show] should produce the following. *)
 
  module ShowFloatInstance = struct
    let float : ShowFloat.a Show.s = (module ShowFloat)
    type __imp_instance__ = Show.__imp_policy__ 
  end
end

open M
let () = assert (show 1 = "1")
let () = assert (show 1.2 = "1.2")
