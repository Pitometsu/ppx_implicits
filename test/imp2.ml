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

(* imp_fun [@imp2 Show]  is equivalent with [%imp Show] *)
external _imp_ : 'a = "%identity"
let () = assert ( (_imp_ [@imp2 Show]) 1 = "1" )
let () = assert ( (_imp_ [@imp2 Show]) 1.0 = "1." )

module Y = struct
  module Show = struct
    let list ~_x:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  end
end

open Y

let () = assert ( (_imp_ [@imp2 Show]) [1;2] = "[ 1; 2 ]" )
(*
module Y = struct
  module Show = struct
    let list ~_x:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  end
end

open Y

*)
