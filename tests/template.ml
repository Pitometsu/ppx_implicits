(*
  Try to share the same code using functor.
*)

module Make(A : sig
  type 'a t
  (* type __imp_policy__ *)
end) = struct
  [@@@warning "-16"]
  type 'a __imp__ = Packed of 'a A.t
  (* Zuut, we cannot copy the policy! *)
  (* type __imp_policy__ = A.__imp_policy__ *) 
  module Instance = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end
  let unpack_opt ?_imp = match _imp with
    | None -> assert false
    | Some (Packed x) -> x
end

module Show = struct
  include Make(struct  type 'a t = 'a -> string end)
  [%%imp_policy Show.Instance, opened ShowInstance]
end

let show = Show.unpack_opt

module Int = struct
  module ShowInstance = struct
    let int = string_of_int
  end
end

open Int

let () = assert (show 1 = "1")
