(* Test of just *)

type 'a show = ('a -> string, [%imp_spec just Show]) Ppx_implicits.Runtime.t
let show : ?_d:'a show -> 'a -> string = Ppx_implicits.Runtime.imp

module Show = struct
  let int = string_of_int

  module Sub = struct
    let int' (x : int) = "42" (* this should NOT be used *)
  end
end

let () = assert ( show 1 = "1" )
