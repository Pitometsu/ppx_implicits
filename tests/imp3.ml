type 'a show = ('a -> string, [%imp Show]) Ppx_implicits.t
    
let show : ?d:'a show -> 'a -> string = Ppx_implicits.imp

module Show = struct
  let int = string_of_int
  let float = string_of_float
  let list : 'a show -> 'a list -> string = fun d xs ->
    "[ " ^ String.concat "; " (List.map (show ~d) xs) ^ " ]"
end

let () = assert (Show.int 1 = "1")
let () = assert (Show.float 1.0 = "1.")
let () = assert (Show.(list (Ppx_implicits.embed int)) [1;2] = "[ 1; 2 ]")

let () = assert (show 1 = "1")
let () = assert (show 1.0 = "1.")
let () = assert (show [1;2] = "[ 1; 2 ]")

(* This does not work since imp does not have type Ppx_implicit.t
let show_twice ?d x = show x ^ show x
*)

let show_twice ?d x = show ?d x ^ show ?d x

let () = assert (show_twice 1 = "11")
let () = assert (show_twice 1.0 = "1.1.")
let () = assert (show_twice [1;2] = "[ 1; 2 ][ 1; 2 ]")

let show_twice ?d:(_ : 'a show option) (x : 'a) = show x ^ show x
  
let () = assert (show_twice 1 = "11")
let () = assert (show_twice 1.0 = "1.1.")
let () = assert (show_twice [1;2] = "[ 1; 2 ][ 1; 2 ]")

(* Forgetting to specify the relation with imp and x *)
let show_twice ?d:(_ : 'a show option) x = show x ^ show x

(* This is now rejected correctly:

Error: File "imp3.ml", line 35, characters 17-27:
  The experssion has type 'a -> string which is too ambiguous to resolve this implicit.
  The following instances may cause infinite loop of the resolution:
    Show.list

We should print the strange type of show_twice to help the understanding of the error.

let () = assert (show_twice 1 = "11")       (* ?d should have a free type variable there it must fail *)
*)
  
