(* 
   
   Let's start to have functions to show various data types in one module Show.

*)

module ShowClass = struct
  type 'a __imp__ = Packed of ('a -> string)
  [%%imp_policy opened ShowInstance]        
  module Instances = struct
    let pack ~_d = Packed _d
    let pack_opt ~_d = Some (pack _d)
  end
  let unpack_opt = function None -> assert false | Some (Packed x) -> x
end
    
let show ?_imp = ShowClass.unpack_opt _imp

module ShowInstance = struct
  let int = string_of_int
  let float = string_of_float
  let list ?_imp:(imp : 'a ShowClass.__imp__ option) (xs : 'a list) = 
    "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
end

let () = assert (show [1] = "[ 1 ]")
