(* auto conversion of [%derive.xxx: _] => [%derive.xxx: ty] *)

let () = assert ([%imp ppxderive ([%derive.show: _] : 'a -> string)] 42 = "42")
(* should expand to 
     [%derive.show: int] 42 = "42"
*)

module ShowClass : sig
  type 'a t = Packed of ('a -> string)
  [%%imp_spec ppxderive ([%derive.show: _] : 'a -> string)]
  module Instances : sig
    val pack : _x:'a -> 'a t
  end
end = struct
  type 'a t = Packed of ('a -> string)
  [%%imp_spec ppxderive ([%derive.show: _] : 'a -> string)]
  module Instances = struct
    let pack ~_x = Packed _x
  end
end

let show ?_d = match _d with
  | None -> assert false
  | Some (ShowClass.Packed f) -> f
