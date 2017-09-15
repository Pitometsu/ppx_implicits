(* "Unshadowing" test *)

type 'a show = ('a -> string, [%imp opened Show]) Ppx_implicits.t
let show : ?d:'a show -> 'a -> string = Ppx_implicits.imp

module X = struct
  module Show = struct
    let int = string_of_int
  end
end

open X (* Add [Show] to the candidate space for [show] *)

module Y = struct
  module X = struct end (* This makes [X.Show.int] inaccessible *)
  let () = assert (show 1 = "1")
end
