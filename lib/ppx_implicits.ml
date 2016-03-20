type ('a, 'spec) t = 'a

exception Not_resolved

let from_Some = function
  | Some x -> x
  | None -> raise Not_resolved
      
external get : ('a, 'spec) t -> 'a = "%identity"

let imp ?d = from_Some d

external embed : 'a -> ('a, 'spec) t = "%identity"
