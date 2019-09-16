module type Eq = sig
  type a
  val (===) : a -> a -> bool
  val (=/=) : a -> a -> bool
end [@@typeclass]

module EqBool = struct
  let (===) = (=)
  let (=/=) = (<>)
end [@@instance: (module Eq with type a = bool)]

module EqInt = struct
  let (===) = (=)
  let (=/=) = (<>)
end [@@instance: (module Eq with type a = int)]

module EqFloat = struct
  let (===) = (=)
  let (=/=) = (<>)
end [@@instance: (module Eq with type a = float)]

module EqList(A : Eq [@typeclass a' Eq]) = struct
  let (===) x y = List.for_all2 A.(===) x y
  let (=/=) x y = List.exists2 A.(=/=) x y
end [@@instance: (module Eq with type a = a' list)]
(* CR jfuruse: we cannot write [@typeclass a Eq] since [a] crashes...
*)

module TestEq = struct
  open Eq
  
  let () =
    assert (1 === 1);
    assert ([1;2;3] === [1;2;3])
end
  
(* Future work. Minimal complete definition support

  let (===) x y = not (x =/= y)
  let (=/=) x y = not (x === y)
*)

type ordering = LT | EQ | GT

(* TODO: class Eq a => Ord a *)
module type Ord = sig
  type a
  val compare : a -> a -> ordering
  val (<=?) : a -> a -> bool
  val (<?) : a -> a -> bool  
  val (>=?) : a -> a -> bool  
  val (>?) : a -> a -> bool
  val max : a -> a -> a
  val min : a -> a -> a
end [@@typeclass]

module OrdInt = struct
  let compare x y = match compare x y with
    | 0 -> EQ
    | 1 -> GT
    | -1 -> LT
    | _ -> assert false
  let (<=?) (x : int) y = x <= y        
  let (<?) (x : int) y = x < y        
  let (>=?) (x : int) y = x >= y        
  let (>?) (x : int) y = x > y        
  let max (x : int) y = max x y
  let min (x : int) y = min x y
end [@@instance: (module Ord with type a = int)]

module OrdFloat = struct
  let compare x y = match compare x y with
    | 0 -> EQ
    | 1 -> GT
    | -1 -> LT
    | _ -> assert false
  let (<=?) (x : float) y = x <= y        
  let (<?) (x : float) y = x < y        
  let (>=?) (x : float) y = x >= y        
  let (>?) (x : float) y = x > y        
  let max (x : float) y = max x y
  let min (x : float) y = min x y
end [@@instance: (module Ord with type a = float)]

module TestOrd = struct
  let () = assert (Ord.compare 1 1 = EQ)

  (* CR jfuruse: Eq a => Ord a therefore we need not a dispatch of Eq ... *)    
  let f ?_imp:(_ : 'a Ord._class option) ?_imp2:(_ : 'a Eq._class option) (x : 'a) y =
    Ord.compare x y = EQ
    && Eq.(===) x y 
end

(*
module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Monad = struct
  type 'a _module = (module Monad with type 'a t = 'a)
end
*)
  

