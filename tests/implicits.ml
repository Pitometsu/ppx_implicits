module Make(A : sig
  type 'a t
end) = struct
  [@@@warning "-16"]
  type 'a t = Packed of 'a A.t
  module Instances = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end
  let unpack_opt ?_imp = match _imp with
    | None -> assert false
    | Some (Packed x) -> x
end

(* Less boxing than Make *)
module Make2(A : sig
  type 'a t
end) : sig
  type 'a t = private 'a A.t
  module Instances : sig
    val pack : _x:'a A.t -> 'a t
    val pack_opt : _x:'a A.t -> 'a t option
  end
  val unpack_opt : ?_imp:'a t -> 'a A.t
end = struct
  [@@@warning "-16"]
  type 'a t = 'a A.t
  module Instances = struct
    let pack ~_x:(_x:'a A.t) = _x
    let pack_opt ~_x:(_x:'a A.t) = Some _x
  end
  let unpack_opt ?_imp = match _imp with
    | None -> assert false
    | Some x -> (x : 'a A.t)
end
  
