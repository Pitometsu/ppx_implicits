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

type 'a show = ('a -> string, [%imp_spec aggressive(related)]) Ppx_implicits.t

let show : ?imp:'a show -> 'a -> string = Ppx_implicits.imp

let () = assert (show (Y.Boo X.Foo) = "Boo (Foo)")
