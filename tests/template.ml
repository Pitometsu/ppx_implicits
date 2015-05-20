(*
  Try to share the same code using functor.
*)

module Make(A : sig
  type 'a t
  (* type __imp_policy__ *)
end) = struct
  type 'a __imp__ = Packed of 'a A.t
  (* Zuut, we cannot copy the policy! *)
  (* type __imp_policy__ = A.__imp_policy__ *) 
  module Show = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end
  let unpack_opt = function
    | None -> assert false
    | Some (Packed x) -> x
  let f ?imp = unpack_opt imp
end

module ShowClass = struct
  include Make(struct  type 'a t = 'a -> string end)
  [%%imp_policy ShowClass.Show, opened Show]
end

let show = ShowClass.f

module Int = struct
  module Show = struct
    let int = string_of_int
  end
end

open Int

let () = assert (show 1 = "1")
