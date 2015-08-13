(* The following strange recipes cannot be composed recursively, 
   since their recusrive use do not strictly decrease the type size.

   Try

   $ ../ppx/ppx_implicits.exe -debug -debug-resolve adm.ml
*)
module Show = struct
  let int ~_d:(_d : int -> string) = _d
  let id ~_d = _d
end

let () = print_string @@ [%imp Show] 1
