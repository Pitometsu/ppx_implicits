(* The following strange recipes cannot be composed recursively, 
   since their recusrive use do not strictly decrease the type size.
*)
type 'a show = ('a -> string, [%imp Show]) Ppx_implicits.t
let show : ?d:'a show -> 'a -> string = Ppx_implicits.imp

module Show = struct
  let int = string_of_int
  let id : ?d:'a show -> 'a -> string = Ppx_implicits.imp
end

let () = print_string @@ show 1
