(*
  aggressive + related: construct overloading instances from normal functions
*)

module X = struct
  type t = Foo
  let show = function
    | Foo -> "Foo"
end

module Y = struct
  type 'a t = Boo of 'a 
  let show show_a = function
    | Boo a -> Printf.sprintf "Boo (%s)" @@ show_a a
end

let () = assert ([%imp aggressive(related)] (Y.Boo X.Foo) = "Boo (Foo)")

module Show = struct
  type 'a t = Packed of ('a -> string)
  [%%imp_spec aggressive(name "show" related)]
  (* default instances *)
  module Instances = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end
  let unpack_opt = function
    | None -> assert false
    | Some (Packed x) -> x
end

let show ?_x = Show.unpack_opt _x

let () = assert (show (Y.Boo X.Foo) = "Boo (Foo)")

