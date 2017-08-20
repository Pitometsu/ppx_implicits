(* The following strange recipes cannot be composed recursively, 
   since their recusrive use do not strictly decrease the type size.

   Try

   $ ../ppx/ppx_implicits.exe -debug -debug-resolve adm.ml
*)
type 'a show = ('a -> string, [%imp Show]) Ppx_implicits.t
let show : ?d:'a show -> 'a -> string = Ppx_implicits.imp

module Show = struct
  let int = string_of_int
  let id : 'a list show -> 'a -> string = fun d x -> show ~d [x]
end

let () = assert (show 1 = "1")

