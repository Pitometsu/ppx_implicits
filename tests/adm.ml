(* The following strange recipes cannot be composed recursively, 
   since their recusrive use do not strictly decrease the type size.

   Try

   $ ../ppx/ppx_implicits.exe -debug -debug-resolve adm.ml
*)
module Show = struct
  let int ~_d:(_d : int -> string) = _d
  let id ~_d = _d
end

type 'a show = ('a -> string, [%imp_spec Show]) Ppx_implicits.Runtime.t
let show : ?_d:'a show -> 'a -> string = Ppx_implicits.Runtime.imp

let () = print_string @@ show 1
