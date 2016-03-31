module type Sum = sig
  type c
  type n
  val sum : c -> n
end [@@typeclass]

module SumList(Num : Numc.Num [@typeclass a Numc.Num]) (* : Sum with type c = Num.a list and type n = Num.a *) = struct
  open Num
  let sum = List.fold_left (+) zero
end [@@instance: (module Sum with type c = a list and type n = a)]

(*
module SumListInstance = struct
  let dict (type a) ?d:(d : (a Numc.Num._module, [%imp_spec has_type Numc.Num.__class__]) Ppx_implicits.t option) =
    let module S = SumList((val (Ppx_implicits.(get (from_Some d))))) in
    ((module S) : (S.c, S.n) Sum._module)
  type __imp_instance_of__ = Sum.__class__
end
*)
  
module SumArray(Num : Numc.Num [@typeclass a Numc.Num]) = struct
  open Num
  let sum = Array.fold_left (+) zero
end [@@instance: (module Sum with type c = a array and type n = a)]

(*
module SumArrayInstance = struct
  let dict (type a) ?d:(d : (a Numc.Num._module, [%imp_spec has_type Numc.Num.__class__]) Ppx_implicits.t option) =
    let module S = SumArray((val (Ppx_implicits.(get (from_Some d))))) in
    ((module S) : (S.c, S.n) Sum._module)
  type __imp_instance_of__ = Sum.__class__
end
*)
  
open Numc (* We need to open this to use Num instances *)
  
let () = assert (Sum.sum [1;2;3] = 6)
let () = assert (Sum.sum [1.0;2.0;3.0] = 6.0)
let () = assert (Sum.sum [|1;2;3|] = 6)
let () = assert (Sum.sum [|1.0;2.0;3.0|] = 6.0)
  
