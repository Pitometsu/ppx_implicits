module M = struct
  module Show = struct
    type 'a __imp__ = private 'a
    external pack : _x:'a -> 'a __imp__ = "%identity"
  end
end
    
let show (type a) imp = let imp = (imp : a M.Show.__imp__ :> a) in imp

module X = struct
  module Show = struct
    let string x = Printf.sprintf "%S" x
    let int = string_of_int
    let float = string_of_float
  end
end

open M
open X

external _imp_ : 'a = "%identity"
let () = assert (show (_imp_ [@imp2 Show]) 1 = "1")
let () = assert (show (_imp_ [@imp3]) 1 = "1")


let show (type a) ?imp = match imp with
  | None -> assert false
  | Some imp -> (imp : a M.Show.__imp__ :> a)

let () = assert (show ?imp:(Some (_imp_ [@imp3])) 1 = "1")
let () = assert (show ?imp:None 1 = "1")
