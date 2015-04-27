(*
  ocamlc -ppx ../ppx/ppx_typeclass -c show.ml
  ocamlc -ppx ../ppx/ppx_typeclass -c separate_compilation.ml
*)

open Show
let () = assert ((Show.show ?_d:(Some Instance.int) 1) = "1")
let () = assert ((Show.show ?_d:(Some Instance.float) 1.0) = "1.")
let () =
  assert
    ((Show.show ?_d:(Some (Instance.list ~_d:Instance.int)) [1; 2; 3]) =
       "[ 1; 2; 3 ]")
let () =
  assert
    ((Show.show
        ?_d:(Some (Instance.list ~_d:(Instance.list ~_d:Instance.int)))
        [[1]; [2; 3]; [4; 5; 6]])
       = "[ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ]")
