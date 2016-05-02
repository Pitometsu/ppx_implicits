type 'a show = ('a -> string, [%imp Show]) Ppx_implicits.t
    
let show : ?d:'a show -> 'a -> string = Ppx_implicits.imp

module Show = struct
  let int = string_of_int
  let float = string_of_float
  let list : ?d:'a show -> 'a list -> string = fun ?d xs ->
    "[ " ^ String.concat "; " (List.map (show ?d) xs) ^ " ]"
end

let () = assert (show 1 = "1")
let () = assert (show 1.0 = "1.")
let () = assert (show [1;2] = "[ 1; 2 ]")

let show_twice ?d x = show ?d x ^ show ?d x

let () = assert (show_twice 1 = "11")
let () = assert (show_twice 1.0 = "1.1.")
let () = assert (show_twice [1;2] = "[ 1; 2 ][ 1; 2 ]")

let show_twice ?d:(_ : 'a show option) (x : 'a) = show x ^ show x
  
let () = assert (show_twice 1 = "11")
let () = assert (show_twice 1.0 = "1.1.")
let () = assert (show_twice [1;2] = "[ 1; 2 ][ 1; 2 ]")
