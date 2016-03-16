(* "Unshadowing" test *)

type 'a show = ('a -> string, [%imp_spec opened Show]) Ppx_implicits.t
let show : ?imp:'a show -> 'a -> string = Ppx_implicits.imp

module X = struct
  module Show = struct
    let int = string_of_int
  end
end

open X

module Y = struct
  module Show = struct
    (* This makes X.Show.int inaccessible *)
  end 
  module X = struct
  end
  let () = assert (show 1 = "1")
end
