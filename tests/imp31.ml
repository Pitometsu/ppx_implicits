module M = struct
  module Show = struct
    type 'a __imp__ = private 'a -> string
    [%%imp_policy opened Show]
    external pack : _x:('a -> string) -> 'a __imp__ = "%identity"
    let pack_opt _x = Some (pack _x)
  end
end
    
let show (type a) ?imp = match imp with
  | None -> assert false
  | Some imp -> (imp : a M.Show.__imp__ :> a -> string)

module X = struct
  module Show = struct
    let string x = Printf.sprintf "%S" x
    let int = string_of_int
    let float = string_of_float
  end
end

open M
open X

(*
let show' ?imp:(i : 'a M.Show.__imp__ option) (x : 'a)  =
  let module Z = struct
    module Show = struct
      let i = match i with
      | None -> assert false
      | Some i -> (i : 'a M.Show.__imp__ :> 'a -> string) 
    end
  end in
  let open Z in
  show ?imp:None x

let () = assert (show' 1 = "1")
*)
