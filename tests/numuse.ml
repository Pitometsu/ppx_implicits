(* Using implicits defined outside of the compilation unit *)
open Numdef

let () =
  assert ((+) 2 2 = 4);
  assert ((+) 1.2 1.2 = 2.4)
