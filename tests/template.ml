(*
  Try to share the same code using functor.
*)

module Show = struct
  [%%imp_spec opened ShowInstance]
  include Ppx_implicits.Implicits.Make(struct type 'a t = 'a -> string end)
  let show = unpack_opt
end


module Int = struct
  module ShowInstance = struct
    let int = string_of_int
  end
end

open Int

let () = assert (Show.show 1 = "1")
