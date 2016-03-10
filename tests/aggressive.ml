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

type 'a show = ('a -> string, [%imp_spec aggressive(related)]) Ppx_implicits.Runtime.t

let show : ?_d:'a show -> 'a -> string = Ppx_implicits.Runtime.imp

let () = assert (show (Y.Boo X.Foo) = "Boo (Foo)")
