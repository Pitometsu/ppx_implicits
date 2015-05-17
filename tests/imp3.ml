module M = struct
  module Show = struct
    type 'a __imp__ = private 'a -> string
    external pack : _x:('a -> string) -> 'a __imp__ = "%identity"
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

let () = assert (show [%imp2 Show] 1 = "1")
let () = assert (show [%imp3] 1 = "1")


let show (type a) ?imp = match imp with
  | None -> assert false
  | Some imp -> (imp : a M.Show.__imp__ :> a -> string)

let () = assert (show ?imp:(Some [%imp3]) 1 = "1")
let () = assert (show ?imp:None 1 = "1")
let () = assert (show 1 = "1")

(* derived *)
  
let show_twice ?imp x = show ?imp x ^ show ?imp x
let () = assert (show_twice 1 = "11")
