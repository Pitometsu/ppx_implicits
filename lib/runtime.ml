type ('a, 'spec) t = 'a

type ('a, 'spec) s = ?_d: ('a, 'spec) t -> 'a

exception Not_resolved

let from_Some = function
  | Some x -> x
  | None -> raise Not_resolved
      
let imp ?_d = from_Some _d

let embed x = Some x
