(* The following strange recipes cannot be composed recursively, 
   since their recusrive use do not strictly decrease the type size.

   Try

   $ ../ppx/ppx_implicits.exe -debug -debug-resolve adm.ml
*)
type 'a show = ('a -> string, [%imp_spec Show]) Ppx_implicits.t
let show : ?imp:'a show -> 'a -> string = Ppx_implicits.imp

module Show = struct
  let int = string_of_int
  let id : ?imp:'a show -> 'a -> string = Ppx_implicits.imp
end

let () = print_string @@ show 1
