module type Num = sig
  type a
  val (+) : a -> a -> a
  val (-) : a -> a -> a
  val ( * ) : a -> a -> a
  val (/) : a -> a -> a
  val (~-) : a -> a
  val abs : a -> a
  val signum : a -> a
  val of_int : int -> a
end [@@typeclass]

include Num

module NumInt = struct
  open Pervasives (* recovers monomorphic operators *)
  type a = int
  let (+) = (+)
  let (-) = (-)
  let ( * ) = ( * )
  let (/) = (/)
  let (~-) = (~-)
  let abs = abs
  let signum = function
    | 0 -> 0
    | x when x > 0 -> 1
    | _ -> -1
  external of_int : int -> int = "%identity"
end [@@instance Num]
  
module NumFloat = struct
  type a = float
  let (+) = (+.)
  let (-) = (-.)
  let ( * ) = ( *. )
  let (/) = (/.)
  let (~-) = (~-.)
  let abs = abs_float
  let signum = function
    | 0.0 -> 0.0
    | x when x > 0.0 -> 1.0
    | _ -> -1.0
  let of_int = float_of_int
end [@@instance Num]
  

