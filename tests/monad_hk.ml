(* Monad class with an encoding of higher kinded polymorphism *)

module HK = struct
  type ('a, 'm) app (* encoding of 'a 'm *)
end
  
module type Monad = sig
  type m
  val return : 'a -> ('a, m) HK.app
  val bind : ('a, m) HK.app -> ('a -> ('b, m) HK.app) -> ('b, m) HK.app
end [@@typeclass]
  
module OptionK = struct
  type t
  external push : 'a option -> ('a, t) HK.app = "%identity"
  external pull : ('a, t) HK.app -> 'a option = "%identity"
end
  
module MonadOption = struct
  type m = OptionK.t
  let return a = OptionK.push @@ Some a
  let bind at f = match OptionK.pull at with
    | None -> OptionK.push None
    | Some a -> f a
end [@@instance Monad]

module ListK = struct
  type t
  external push : 'a list -> ('a, t) HK.app = "%identity"
  external pull : ('a, t) HK.app -> 'a list = "%identity"
end
  
module MonadList = struct
  type m = ListK.t
  let return a = ListK.push @@ [a]
  let bind at f = ListK.push @@ List.(concat @@ map (fun x -> ListK.pull (f x)) (ListK.pull at))
end [@@instance Monad]

let () =
  let open Monad in
  assert (OptionK.pull (return 1) = Some 1)

