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
    
module type Ord = sig
  type a
  module Eq : Eq with type a = a
  val compare : a -> a -> ordering
  val (<=?) : a -> a -> bool
  val (<?) : a -> a -> bool  
  val (>=?) : a -> a -> bool  
  val (>?) : a -> a -> bool
  val max : a -> a -> a
  val min : a -> a -> a
end

(* I got troubled by writing [@@imp_spec typeclass]... It must be [%%imp_spec typeclass] *)
module Ord = struct
  [@@@warning "-16"]
  type 'a _module = (module Ord with type a = 'a)
  type 'a _class = Packed of 'a _module
  [%%imp_spec typeclass]

  let get_eq_module (type a) : a _module -> a Eq._module = fun m ->
    let module M : Ord with type a = a = (val m) in
    (module M.Eq)

  (* Load it as an instance of Eq, not Ord! *)        
  let get_eq_class_opt (type a) : a _class option -> a Eq._class option = fun co ->
    match co with
    | None -> assert false
    | Some (Packed m) -> Some (Eq.Packed (get_eq_module m))
      
  module Instances = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end

  let unpack_opt ?_imp:(_imp as __imp__function__3)  =
    match _imp with | None  -> assert false | Some (Packed x) -> x

  let compare (type a) ?_imp:(_imp as __imp__function__4)  =
      let module M = (val (unpack_opt ?_imp : a _module)) in M.compare
  let (<=?) (type a) ?_imp:(_imp as __imp__function__4)  =
      let module M = (val (unpack_opt ?_imp : a _module)) in M.(<=?)
  let (<?) (type a) ?_imp:(_imp as __imp__function__4)  =
      let module M = (val (unpack_opt ?_imp : a _module)) in M.(<?)
  let (>=?) (type a) ?_imp:(_imp as __imp__function__4)  =
      let module M = (val (unpack_opt ?_imp : a _module)) in M.(>=?)
  let (>?) (type a) ?_imp:(_imp as __imp__function__4)  =
      let module M = (val (unpack_opt ?_imp : a _module)) in M.(>?)
  let max (type a) ?_imp:(_imp as __imp__function__4)  =
      let module M = (val (unpack_opt ?_imp : a _module)) in M.max
  let min (type a) ?_imp:(_imp as __imp__function__4)  =
      let module M = (val (unpack_opt ?_imp : a _module)) in M.min
end

(* A magic way to get the instance of Eq from an instance of Ord *)
module EqFromOrd = struct
  let get_eq_class_opt ~_x = Ord.get_eq_class_opt _x
  type __imp_instance__ = Eq.__imp_spec__
end
  
module OrdInt = struct
  type a = int
  module Eq = EqInt
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
end

module OrdIntInstance = struct
  let dict : OrdInt.a Ord._module = (module OrdInt)
  type __imp_instance__ = Ord.__imp_spec__
end

module TestOrd = struct
  let () = assert (Ord.compare 1 1 = EQ)

  let f ?_imp:(_ : 'a Ord._class option) (x : 'a) y =
    let open Eq in
    Ord.compare x y = EQ
    && x === y (* we need instance for 'a Eq._class option 
                                       'a Eq._class
                                       'a Ord._class option -> 'a Eq._class option
                *)
end
