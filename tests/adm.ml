(* The following strange recipes cannot be composed recursively, 
   since their recusrive use do not strictly decrease the type size.

   Try

   $ ../ppx/ppx_implicits.exe -debug -debug-resolve adm.ml
*)
type 'a show = ('a -> string, [%imp Show]) Ppx_implicits.t
let show : ?d:'a show -> 'a -> string = Ppx_implicits.imp

module Show = struct
  let wrecked : 'a list show -> 'a -> string = fun d x -> show ~d [x]
  let int : int -> string = string_of_int
end

let () = assert (show 1 = "1")
