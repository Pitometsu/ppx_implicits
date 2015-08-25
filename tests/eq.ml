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

module EqFloat = struct
  type a = float
  let (===) = (=)
  let (=/=) = (<>)
end [@@instance Eq]

module EqList(A : Eq) = struct
  type a = A.a list
  let (===) x y = List.for_all2 A.(===) x y
  let (=/=) x y = List.exists2 A.(=/=) x y
end

module EqListInstance = struct
  let list (type a) ~_a:(a : a Eq._module) : a list Eq._module =
    let module A : Eq with type a = a = (val a) in
    (module EqList(A))
  type __imp_instance__ = Eq.__imp_spec__
end
  
  
(* Future work. Minimal complete definition support

  let (===) x y = not (x =/= y)
  let (=/=) x y = not (x === y)
*)
