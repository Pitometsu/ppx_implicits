module ShowClass = struct
  type 'a t = Packed of ('a -> string)
  [%%imp_spec opened ShowInstance]        
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

  (* constraint works! *)
  let list ?_imp:(_ : 'a ShowClass.t option) (xs : 'a list) = 
    "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
end

let () = assert (show [1] = "[ 1 ]")
