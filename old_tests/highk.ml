(* A class to ease higher kinded polymorphism *)

type ('a, 'm) app (* encoding of 'a 'm *)

(* It is as same as "Lightweight Higher Kinded Polymorphism", Jeremy Yallop and Leo White *)
  
module Make(A : sig
  type 'a t
end) = struct
  open A
  type k
  external inj : 'a t -> ('a, k) app = "%identity"
  external prj : ('a, k) app -> 'a t = "%identity"
end
  
module OptionKind = Make(struct type 'a t = 'a option end)
  
module ListKind = Make(struct type 'a t = 'a list end)

module Inj = struct
  type ('at, 'a, 'k) t = Packed of ('at -> ('a, 'k) app)
  [%%imp_spec aggressive(name "inj" related)]
  module Instances = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end
  let unpack_opt = function
    | None -> assert false
    | Some (Packed x) -> x
end

let inj ?_x = Inj.unpack_opt _x

module Prj = struct
  type ('at, 'a, 'k) t = Packed of (('a, 'k) app -> 'at)
  [%%imp_spec aggressive(name "prj" related)]
  module Instances = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end
  let unpack_opt = function
    | None -> assert false
    | Some (Packed x) -> x
end

let prj ?_x = Prj.unpack_opt _x

let () =
  assert (prj (inj (Some 1)) = Some 1)
    
