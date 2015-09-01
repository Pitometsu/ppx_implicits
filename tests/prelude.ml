module type Eq = sig
  type a
  val (===) : a -> a -> bool
  val (=/=) : a -> a -> bool
end [@@typeclass]

module EqBool = struct
  type a = bool
  let (===) = (=)
  let (=/=) = (<>)
end [@@instance Eq]

module EqInt = struct
  type a = int
  let (===) = (=)
  let (=/=) = (<>)
end [@@instance Eq]

module EqFloat = struct
  type a = float
  let (===) = (=)
  let (=/=) = (<>)
end [@@instance Eq]

module EqList(A : Eq) = struct
  type a = A.a list
  let (===) x y = List.for_all2 A.(===) x y
  let (=/=) x y = List.exists2 A.(=/=) x y
end (* [@@instance Eq]? *)

(* Can we auto-generate the following from the above ? *)
module EqListInstance = struct
  let list (type a) ~_a:(a : a Eq._module) : a list Eq._module =
    let module A : Eq with type a = a = (val a) in
    (module EqList(A))
  type __imp_instance__ = Eq.__imp_spec__
end
  
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
  type a = int
  let compare x y = match compare x y with
    | 0 -> EQ
    | 1 -> GT
    | -1 -> LT
    | _ -> assert false
  let (<=?) (x : a) y = x <= y        
  let (<?) (x : a) y = x < y        
  let (>=?) (x : a) y = x >= y        
  let (>?) (x : a) y = x > y        
  let max (x : a) y = max x y
  let min (x : a) y = min x y
end [@@instance Ord]

module OrdFloat = struct
  type a = float
  let compare x y = match compare x y with
    | 0 -> EQ
    | 1 -> GT
    | -1 -> LT
    | _ -> assert false
  let (<=?) (x : a) y = x <= y        
  let (<?) (x : a) y = x < y        
  let (>=?) (x : a) y = x >= y        
  let (>?) (x : a) y = x > y        
  let max (x : a) y = max x y
  let min (x : a) y = min x y
end [@@instance Ord]

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
  
