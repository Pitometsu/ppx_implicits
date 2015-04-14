module Num = struct
  module type Num = sig
    type a
    val plus : a -> a -> a
  end

  let plus (type a) ?_d = match _d with
    | None -> assert false
    | Some _d ->
        let module D = (val (_d : (module Num with type a = a))) in D.plus

  (* This is inefficient. This should be replaced by the following,
     so that it could be directly replaced by the instance value:

     plus => let plus = let module Num = (val Instance.int) in Num.plus in plus
     
     external plus : ?_d:(module Num with type a = 'a) -> 'a -> 'a -> 'a = "%OVERLOADED"
  *)
end

module Int = struct
  type a = int
  let plus = (+)
end

module Float = struct
  type a = float
  let plus = (+.)
end

module Instance = struct
  let int = (module Int : Num.Num with type a = int)
  let float = (module Float : Num.Num with type a = float)

  (* I believe currently there is no good exposed API to compare
     a packed module type and a module type.
     Dictionaries have to be translated to the first order module values
     so that we can use the normal type unification 
  *)
end

let () = assert (Num.plus 1 2 = 3)
let () = assert (Num.plus 1.2 3.4 = 4.6)

(* With an explicit dispatch code, the derived overloading works! *)
let double ?_d x = Num.plus ?_d x x
let () = assert (double 2 = 4)
