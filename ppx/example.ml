(* 

$ ocamlc -ppx ppx_typeclass example.ml
$ ./a.out

or

$ ./ppx_typeclass -debug example.ml

*)

module Num = struct
  module type Num = sig
    type a
    val (+) : a -> a -> a
    val (-) : a -> a -> a
  end

  let (+) (type a) ?_d = match _d with
    | None -> assert false
    | Some _d ->
        let module D = (val (_d : (module Num with type a = a))) in D.(+)

  (* This is inefficient. This should be replaced by the following,
     so that it could be directly replaced by the instance value:

     external plus : ?_d:(module Num with type a = 'a) -> 'a -> 'a -> 'a = "%OVERLOADED"

     plus => let plus = let module Num = (val Instance.int) in Num.plus in plus
  *)

  let (-) (type a) ?_d = match _d with
    | None -> assert false
    | Some _d ->
        let module D = (val (_d : (module Num with type a = a))) in D.(-)

end

module Int = struct
  type a = int
  let (+) = (+)
  let (-) = (-)
end

module Float = struct
  type a = float
  let (+) = (+.)
  let (-) = (-.)
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

let () = assert (Num.(+) 1 2 = 3)
let () = assert (Num.(+) 1.2 3.4 = 4.6)
let () = assert (Num.(-) 2 1 = 1)

(* With an explicit dispatch code, the derived overloading works! *)
let double ?_d x = Num.(+) ?_d x x
let () = assert (double 2 = 4)
