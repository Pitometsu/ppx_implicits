module M = struct
  module Show : sig
    [%%imp_pack.t: 'a -> string]
    [%%imp_spec opened Show]
  end = struct
    [%%imp_pack.t: 'a -> string]
    [%%imp_spec opened Show]
  end
end
    
let show (type a) imp = (imp : a M.Show.t :> a -> string)

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
  | Some imp -> (imp : a M.Show.t :> a -> string)

let () = assert (show ?_imp:(Some [%imp]) 1 = "1")
let () = assert (show ?_imp:None 1 = "1")
let () = assert (show 1 = "1")

(* derived *)
  
let show_twice ?_imp x = show ?_imp x ^ show ?_imp x
let () = assert (show_twice 1 = "11")

(* derived auto *)
let show_twice ?_imp:(imp : 'a M.Show.t option) (x : 'a) =
  show x ^ show x
    
let () = assert (show_twice 1 = "11")

