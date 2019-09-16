module X = struct
  module Show = struct
    let string x = Printf.sprintf "%S" x
    let int = string_of_int
    let float = string_of_float
  end
end

open X

(* 4.02.1: (fun p -> e) does not keep attributes.
   Therefore we cannot write (fun _ -> assert false) [@imp2 Show] 
 *)

(* If you cannot use [%imp opened Show], you can always replace it by (assert false) [@imp2 Show] *)
let () = assert ( [%imp opened Show] 1 = "1" )
let () = assert ( [%imp opened Show] 1.0 = "1." )

module Y = struct
  module Show = struct
    let list ~_x:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  end
end

open Y

let () = assert ( [%imp opened Show] [1;2] = "[ 1; 2 ]" )

(* derived *)
  
let show_twice imp x = imp x ^ imp x
let () = assert ( show_twice [%imp opened Show] [1;2] = "[ 1; 2 ][ 1; 2 ]" )
