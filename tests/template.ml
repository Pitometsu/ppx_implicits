(*
  Try to share the same code using functor.
*)

module Show = struct
  include Ppx_implicits.Implicits.Make(struct type 'a t = 'a -> string end)
  [%%imp_policy Show.Instance, opened ShowInstance]
end

let show = Show.unpack_opt

module Int = struct
  module ShowInstance = struct
    let int = string_of_int
  end
end

open Int

let () = assert (show 1 = "1")
