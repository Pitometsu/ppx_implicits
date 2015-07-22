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

  [%%imp_policy opened ShowInstance]

  let show (type a) ?_imp = let module M = (val (unpack_opt ?_imp : a s)) in M.show
end

let show = Show.show

module Int = struct
  module ShowInt = struct
    type a = int
    let show  = string_of_int
  end

  (* [%implicit Show] must change the above definition to the following 

    module ShowInt : Show with type a = int = struct
      type a = int
      let show  = string_of_int
    end
  *)

  (* The above with [%implicit Show] should produce the following. *)
 
  module ShowInstance = struct
    let int : int Show.s = (module ShowInt)
  end
end

open Int
let () = assert (show 1 = "1")
