(* To check overloading from the outer modules works *)

open Show

let () = assert (Show.show 1 = "1")
let () = assert (Show.show 1.0 = "1.")
let () = assert (Show.show [1;2;3] = "[ 1; 2; 3 ]") 
let () = assert (Show.show [[1]; [2;3]; [4;5;6]] = "[ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ]") 
