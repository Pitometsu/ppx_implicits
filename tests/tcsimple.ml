module type Num = sig
  type a
  val (+) : a -> a -> a
  val (-) : a -> a -> a
end

type 'a t = (module Num with type a = 'a)
    
module NumInstances = struct
  let int : int t = (module struct
    type a = int
    let (+) = Pervasives.(+)
    let (-) = Pervasives.(-)
  end)	

  let float : float t = (module struct
    type a = float
    let (+) = Pervasives.(+.)
    let (-) = Pervasives.(-.)
  end)	
end

let plus (type a) ?_d x y = match _d with
  | None -> assert false
  | Some d -> 
      let module M = (val (d : a t) ) in
      M.(+) x y

    

