module M1 = struct

  module Show = struct
    let int = string_of_int
    let float = string_of_float
    let list ~_d xs = "[ " ^ String.concat "; " (List.map _d xs) ^ " ]"
    let hoge ~_b ~_a (a,b) = "(" ^ _a a ^ ", " ^ _b b ^ ")"
  end
    
  module ShowClass = struct
    type 'a t = Packed of ('a -> string)
    [%%imp_spec Show]
    module Instances = struct
      let pack ~_d:f = Packed f
    end
  end
  
  (* val show : 'a ShowClass.t -> 'a -> string *)
  let show (ShowClass.Packed f) = f
  
  let () = assert (show [%imp] [1; 2; 3] = "[ 1; 2; 3 ]")
  let () = assert (show [%imp] (1,2) = "(1, 2)")
  let () = assert (show [%imp] (1,2.3) = "(1, 2.3)")
  let () = assert (show [%imp] (1,true) = "(1, true)")
end


module M2 = struct


  module Show = struct
    let int = string_of_int
    let float = string_of_float
    let list ~_d xs = "[ " ^ String.concat "; " (List.map _d xs) ^ " ]"
  end
    
  module ShowClass = struct
    type 'a t = Packed of ('a -> string)
    [%%imp_spec Show]
    module Instances = struct
      let pack ~_d:f = Packed f
      let pack_opt ~_d:f = Some (Packed f)
    end
  end
  
  (* val show : 'a ShowClass.t -> 'a -> string *)
  let show ?_d = match _d with
    | None -> assert false
    | Some (ShowClass.Packed f) -> f
  
  let () = assert (show [1; 2; 3] = "[ 1; 2; 3 ]")

    
end
  
