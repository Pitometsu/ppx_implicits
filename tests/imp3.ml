module M = struct
  module Show = struct
    type 'a __imp__ = private 'a -> string
    [%%imp_policy opened Show]
    external pack : _x:('a -> string) -> 'a __imp__ = "%identity"
    let pack_opt ~_x = Some (pack ~_x)
  end
end
    
let show (type a) imp = (imp : a M.Show.__imp__ :> a -> string)

module X = struct
  module Show = struct
    let string x = Printf.sprintf "%S" x
    let int = string_of_int
    let float = string_of_float
  end
end

open M
open X

let () = assert (show [%imp opened Show] 1 = "1")
let () = assert (show [%imp] 1 = "1")


let show (type a) ?_imp = match _imp with
  | None -> assert false
  | Some imp -> (imp : a M.Show.__imp__ :> a -> string)

let () = assert (show ?_imp:(Some [%imp]) 1 = "1")
let () = assert (show ?_imp:None 1 = "1")
let () = assert (show 1 = "1")

(* derived *)
  
let show_twice ?_imp x = show ?_imp x ^ show ?_imp x
let () = assert (show_twice 1 = "11")

(* derived auto *)
let show_twice ?_imp:(imp : 'a M.Show.__imp__ option) (x : 'a) =
  let module Z = struct
    module Show = struct
      let imp = imp
    end
  end in
  let open Z in
  show x ^ show x
    
let () = assert (show_twice 1 = "11")

