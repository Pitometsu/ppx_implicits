(* The following strange recipes are rejected to be used recursively
   since their recusrive use do not strictly decrease the type size *)
module Show = struct
  let int ~_d:(_d : int -> string) = _d
  let id ~_d = _d
end

let () = print_string @@ [%imp Show] 1
