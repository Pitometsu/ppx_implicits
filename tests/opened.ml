(* test for opened policy *)

module X = struct
  module Show = struct
    let int = string_of_int
  end
end

module Y = struct
  module Show = struct
    let float = string_of_float
  end
end

module ShowClass = struct
  type 'a __imp__ = Packed of ('a -> string)
  [%%imp_policy opened Show]
  module Show = struct
    let pack ~_x:f = Some (Packed f)
  end
  let unpack = function
    | None -> assert false
    | Some (Packed f) -> f
  let show ?_imp = unpack _imp
end

open X
open Y
open ShowClass

let () = 
  assert (show 1 = "1");
  assert (show 1.2 = "1.2")
