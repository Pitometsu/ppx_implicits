module type Sum = sig
  type c
  type n
  val sum : c -> n
end [@@typeclass]

module SumList(Num : Numdef.Num) (* : Sum with type c = Num.a list and type n = Num.a *) = struct
  open Num
  type c = Num.a list
  type n = Num.a
  let sum = List.fold_left (+) zero
end

module SumListInstance = struct
  let dict (type a) ?d:(d : (a Numdef.Num._module, [%imp_spec has_type Numdef.Num.__class__]) Ppx_implicits.t option) =
    let d = Ppx_implicits.(get (from_Some d)) in
    let module D = (val (d : a Numdef.Num._module)) in
    let module S = SumList(D) in
    ((module S) : (a list, a) Sum._module)
  type __imp_instance_of__ = Sum.__class__
end
  
module SumArray(Num : Numdef.Num) = struct
  open Num
  type c = Num.a array
  type n = Num.a
  let sum = Array.fold_left (+) zero
end

module SumArrayInstance = struct
  let dict (type a) ?d:(d : (a Numdef.Num._module, [%imp_spec has_type Numdef.Num.__class__]) Ppx_implicits.t option) =
    let d = Ppx_implicits.(get (from_Some d)) in
    let module D = (val (d : a Numdef.Num._module)) in
    let module S = SumArray(D) in
    ((module S) : (a array, a) Sum._module)
  type __imp_instance_of__ = Sum.__class__
end

open Numdef (* We need to open this to use Num instances *)
  
let () = assert (Sum.sum [1;2;3] = 6)
let () = assert (Sum.sum [1.0;2.0;3.0] = 6.0)
let () = assert (Sum.sum [|1;2;3|] = 6)
let () = assert (Sum.sum [|1.0;2.0;3.0|] = 6.0)
  
