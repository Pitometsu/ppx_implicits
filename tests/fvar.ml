module type Coerce = sig
  type a
  type b
  val coerce : a -> b
end [@@typeclass]

module M = struct

  module IF = struct
    type a = int
    type b = float
    let coerce = float_of_int
  end [@@instance Coerce]

  module FI = struct
    type a = float
    type b = int
    let coerce = int_of_float
  end [@@instance Coerce]
end

open M

module N : sig end = struct
  let x = ref None (* '_a option ref *)

  let () = x := Some (Coerce.coerce 1) (* should instantiate '_a to float *)
end
