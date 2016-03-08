type 'a show = ('a -> string, [%imp_spec Show]) Ppx_implicits.Runtime.t
    
let show : ?_d:'a show -> 'a -> string = Ppx_implicits.Runtime.imp

module Show = struct
  let int = string_of_int
  let float = string_of_float
  let list : ?_d:'a show -> 'a list -> string = fun ?_d xs ->
    "[ " ^ String.concat "; " (List.map (show ?_d) xs) ^ " ]"
end

let () = assert (show 1 = "1")
let () = assert (show 1.0 = "1.")
let () = assert (show [1;2] = "[ 1; 2 ]")

let show_twice ?_d x = show ?_d x ^ show ?_d x

let () = assert (show_twice 1 = "11")
let () = assert (show_twice 1.0 = "1.1.")
let () = assert (show_twice [1;2] = "[ 1; 2 ][ 1; 2 ]")

let show_twice ?_d:(_ : 'a show option) (x : 'a) = show x ^ show x
  
let () = assert (show_twice 1 = "11")
let () = assert (show_twice 1.0 = "1.1.")
let () = assert (show_twice [1;2] = "[ 1; 2 ][ 1; 2 ]")
