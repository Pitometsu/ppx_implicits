type 'a show = ('a -> string, [%imp Show]) Ppx_implicits.t

let show : ?d: 'a show -> 'a -> string = Ppx_implicits.imp

module Show = struct
  let int = string_of_int
  let float = string_of_float
  let a = function (`A : [`A]) -> "`A"
end

let () = assert (show 1 = "1")
let () = assert (show 1.0 = "1.")
let () = assert (show `A = "`A")
