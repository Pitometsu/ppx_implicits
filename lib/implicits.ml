module Make(A : sig
  type 'a t
end) = struct
  [@@@warning "-16"]
  type 'a t = Packed of 'a A.t
  module Instance = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end
  let unpack_opt ?_imp = match _imp with
    | None -> assert false
    | Some (Packed x) -> x
end
