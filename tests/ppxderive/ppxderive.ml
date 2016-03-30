(* auto conversion of show e => [%derive.show: <type_of_e>] e 

   The output requires ppx_deriving to compile. 
   This requires ppx_implicits's -ppx option to execute $(PPX_DERIVING_DIR)/ppx_deriving $(PPX_DERIVING_DIR)/ppx_deriving_show.cma
*)

type 'a show = ('a -> string, [%imp ppxderive ([%derive.show: _] : 'a -> string)]) Ppx_implicits.t

let show : ?d:'a show -> 'a -> string = Ppx_implicits.imp
  
let () = assert (show 42 = "42")
let () = assert (show (42,42) = "(42, 42)")
