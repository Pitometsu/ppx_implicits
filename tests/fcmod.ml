module type Num = sig
  type a
  val (+) : a -> a -> a
  val (-) : a -> a -> a
end

module Num = struct
  type 'a t = Packed of (module Num with type a = 'a)
  [%%imp_spec NumInstances]
end

open Num

module NumInstances = struct
  let pack_opt ~_d = Some _d
    
  let int : int Num.t = Packed (module struct
    type a = int
    let (+) = Pervasives.(+)
    let (-) = Pervasives.(-)
  end)	

  let float : float Num.t = Packed (module struct
    type a = float
    let (+) = Pervasives.(+.)
    let (-) = Pervasives.(-.)
  end)	
end

let plus (type a) ?_d:(_d : a Num.t option) = match _d with
  | None -> assert false
  | Some (Num.Packed d) ->
      let module M = (val d) in
      M.(+)

let () =
  assert (plus 1 2 = 3);
  assert (plus 1.2 3.4 = 4.6)
