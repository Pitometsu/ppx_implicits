module type Sum = sig
  type c
  type n
  val sum : c -> n
end [@@typeclass]

module SumList(Num : Numc.Num [@typeclass a Numc.Num]) (* : Sum with type c = Num.a list and type n = Num.a *) = struct
  open Num
  type c = Num.a list
  type n = Num.a
  let sum = List.fold_left (+) zero
end [@@instance Sum]

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
  type c = Num.a array
  type n = Num.a
  let sum = Array.fold_left (+) zero
end [@@instance Sum]

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
  
