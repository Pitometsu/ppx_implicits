module M = struct
  module Show = struct
    type 'a t = private 'a -> string
    [%%imp_spec opened Show]
    external pack : _x:('a -> string) -> 'a t = "%identity"
    let pack_opt ~_x = Some (pack _x)
  end
end
    
let show (type a) ?_imp = match _imp with
  | None -> assert false
  | Some imp -> (imp : a M.Show.t :> a -> string)

module X = struct
  module Show = struct
    let string x = Printf.sprintf "%S" x
    let int = string_of_int
    let float = string_of_float
  end
end

open M
open X

let show' ?_imp:(i : 'a M.Show.t option) (x : 'a)  =
  show ?_imp:None x

let () = assert (show' 1 = "1")

