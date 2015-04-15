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

  type 'a t = (module Num with type a = 'a)

  let (+) (type a) ?_d = match _d with
    | None -> assert false
    | Some _d ->
        let module D = (val (_d : a t)) in D.(+)

  (* Types for overloading are distinguished from the normal types
     by the special label "_[A-z]+" *)

  (* This is inefficient. This should be replaced by the following,
     so that it could be directly replaced by the instance value:

     external plus : ?_d:(module Num with type a = 'a) -> 'a -> 'a -> 'a = "%OVERLOADED"

     plus => let plus = let module Num = (val Instance.int) in Num.plus in plus
  *)

  let (-) (type a) ?_d = match _d with
    | None -> assert false
    | Some _d ->
        let module D = (val (_d : a t)) in D.(-)

end

(* The above should be auto-generated from the following:

[%% typeclass
module type Num = sig
  type a
  val (+) : a -> a -> a
  val (-) : a -> a -> a
end
]

*)
 
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
  let int : int Num.t = (module Int)
  let float : float Num.t = (module Float)

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

(* We must prevent the following:

  Num.(+) ?_d:(List.hd [None]) 1 2

Since the ppx misunderstands some dictionary is supplied.

What users are allowed to do are:

  * Omit ?_d and ask the ppx fill it:  Num.(+) 1 2
  * Apply just ?_d for explicit dispatching:  Num.(+) ?_d x x   ...   well, still we can write let _d = stupid expression in ... :-(
  * Apply a dictionary explicitly with ~_d:  Num.(+) ~_d:Instance.int 1 2

What users should not do are:

  * Apply some expression with ?_d:   Num.(+) ?_d:(if b then Some Instance.int else None)
*)

