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

type 'a show = ('a -> string, [%imp_spec Show]) Ppx_implicits.t

let show : ?_d:'a show -> 'a -> string = Ppx_implicits.imp

let () = assert (show 1 = "1")
let () = assert (show 1.0 = "1.")
let () = assert (show [1;2] = "[ 1; 2 ]")

let show_twice ?_d x = show ?_d x ^ show ?_d x

let () = assert (show_twice 1 = "11")
let () = assert (show_twice 1.0 = "1.1.")
let () = assert (show_twice [1;2] = "[ 1; 2 ][ 1; 2 ]")

module X = struct
  module Show = Show
end
  
module Y = struct
  module Show = struct
    let tuple ~_d1:show1 ~_d2:show2 (x,y) = "(" ^ show1 x ^ ", " ^ show2 y ^ ")"
  end
end

open X
open Y

type 'a show_opened = ('a -> string, [%imp_spec opened Show]) Ppx_implicits.t

let show : ?_d:'a show_opened -> 'a -> string = Ppx_implicits.imp

let () = assert (show ([(1,2);(2,3)],4.2) = "([ (1, 2); (2, 3) ], 4.2)")
