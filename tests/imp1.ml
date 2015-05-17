module Show = struct
  let string x = Printf.sprintf "%S" x
  let int = string_of_int
  let float = string_of_float
end

(* 4.02.1: (fun p -> e) does not keep attributes.
   Therefore we cannot write (fun _ -> assert false) [@imp Show] 
 *)

(* If you cannot use [%imp Show], you can always replace it by (assert false) [@imp Show] *)
let () = assert ( (assert false[@imp Show]) 1 = "1" )
let () = assert ( (assert false[@imp Show]) 1.0 = "1." )

module Show2 = struct
  (* Currently functions need explicit label start with '_'
     to indicate the argument can be composed using the recipe.
  *)
  let list ~_x:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
end

let () = assert ( [%imp Show, Show2] [1;2] = "[ 1; 2 ]" )

module Show3 = struct
  include Show
  module Show2 = Show2
end

let () = assert ( [%imp Show3] [1;2] = "[ 1; 2 ]" )

(* derived *)
  
let show_twice imp x = imp x ^ imp x
let () = assert ( show_twice [%imp Show3] [1;2] = "[ 1; 2 ][ 1; 2 ]" )

