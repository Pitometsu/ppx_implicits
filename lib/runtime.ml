type ('a, 'spec) t = 'a

exception Not_resolved

let imp = function
  | Some x -> x
  | None -> raise Not_resolved

let embed x = Some x
