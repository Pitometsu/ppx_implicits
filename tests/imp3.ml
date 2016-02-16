module Show = struct
  let int = string_of_int
  let float = string_of_float
  let list ~_d:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  (* A label starts with '_' is a constraint label,
     which is like => arrow of Haskell.
  *)
end

let () = assert (Show.int 1 = "1")
let () = assert (Show.float 1.0 = "1.")
let () = assert (Show.(list ~_d:int) [1;2] = "[ 1; 2 ]")

let show = Ppx_implicits.Runtime.( (imp : ('a -> string, [%imp_spec Show]) s) )

let () = assert (show 1 = "1")
let () = assert (show 1.0 = "1.")
let () = assert (show [1;2] = "[ 1; 2 ]")

(* CR jfuruse: we must reject the following *)
(*
let show_twice ?_d x = show x ^ show x
*)

let show_twice ?_d:(_ : ('a -> string, [%imp_spec Show]) Ppx_implicits.Runtime.t option) x = show x ^ show x
  
let () = assert (show_twice 1 = "11")
let () = assert (show_twice 1.0 = "1.1.")
let () = assert (show_twice [1;2] = "[ 1; 2 ][ 1; 2 ]")
