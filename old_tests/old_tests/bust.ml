(*
  ocamlc -ppx ../ppx/ppx_typeclass -c show.ml
  ocamlc -ppx ../ppx/ppx_typeclass -c separate_compilation.ml
*)

open Show

let () = assert ((Show.show ?_d:None 1) = "1") (* ok *)
let () = assert ((Show.show ?_d:(if true then None else None) 1) = "1") (* bust! *)
let () = assert ((Show.show ~_d:(Instance.int) 1) = "1") (* bust! *)
