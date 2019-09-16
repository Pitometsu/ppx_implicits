(* implicit format!! *)

open Format
  
module Base = struct
  let int ppf = fprintf ppf "%d"
end

module M = struct
  type 'a t = Packed of (formatter -> 'a -> unit)
  [%%imp_spec Base]
  (* default instances *)
  module Instances = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end
  let unpack_opt = function
    | None -> assert false
    | Some (Packed x) -> x
end

let format : ?_x: 'a M.t -> formatter -> 'a -> unit = fun ?_x -> M.unpack_opt _x
  
let f = format

let g ?_x:(_x : 'a M.t option) ppf (a : 'a) = f ?_x ppf a; f ?_x ppf a

let h ?_x:(_x : 'a M.t option) ppf (a : 'a) = f ppf a; f ppf 1

