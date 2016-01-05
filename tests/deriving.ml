module Show = struct
  let tuple ~_ds:ds (vs : 'a) : string =
    let vs = Obj.repr vs in
    assert (Obj.is_block ds);
    assert (Obj.is_block vs);
    let s = Obj.size ds in
    let s' = Obj.size vs in
    assert (s = s');
    let rec ss n =
      if n = s then []
      else
        let f : 'a -> string = Obj.obj (Obj.field ds n) in
        let v : 'a = Obj.obj (Obj.field vs n) in
        f v :: ss (n+1)
    in
    "(" ^ String.concat "," (ss 0) ^ ")"
end

module Base = struct
  let show_of_int = string_of_int
end

(*
module Show = struct
  type 'a t = Packed of ('a -> string)
  [%%imp_spec aggressive(name "show" related), Base]
  (* default instances *)
  module Instances = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end
  let unpack_opt = function
    | None -> assert false
    | Some (Packed x) -> x
end
*)
  
let () = assert ([%imp aggressive(name "show" related), Base] 42 = "42")
let () = assert ([%imp aggressive(name "show" related), deriving Show, Base] (42,42) = "(42,42)")
