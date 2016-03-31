module type Coerce = sig
  type a
  type b
  val coerce : a -> b
end [@@typeclass]

(* Instance defined not within a submodule *)
  
module IF = struct
  let coerce = float_of_int
end [@@instance: (module Coerce with type a = int and type b = float)]

module FI = struct
  let coerce = int_of_float
end [@@instance: (module Coerce with type a = float and type b = int)]

module N : sig end = struct
  let x = ref None (* '_a option ref *)

  let () = x := Some (Coerce.coerce 1) (* should instantiate '_a to float *)
end
