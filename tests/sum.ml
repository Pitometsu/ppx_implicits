(* Instance with constraint dependency *)

module type Sum = sig
  type c
  type n
  val sum : c -> n
end [@@typeclass]

module SumList(Num : Numc.Num [@typeclass a Numc.Num]) = struct
  open Num
  let sum = List.fold_left (+) zero
end [@@instance: (module Sum with type c = a list and type n = a)]
    (* In [@@instance], we can use the paramter [a] of [Numc.Num] *)

module SumArray(Num : Numc.Num [@typeclass a Numc.Num]) = struct
  open Num
  let sum = Array.fold_left (+) zero
end [@@instance: (module Sum with type c = a array and type n = a)]
    (* In [@@instance], we can use the paramter [a] of [Numc.Num] *)

open Numc (* We need to open this to use Num instances *)
  
let () = assert (Sum.sum [1;2;3] = 6)
let () = assert (Sum.sum [1.0;2.0;3.0] = 6.0)
let () = assert (Sum.sum [|1;2;3|] = 6)
let () = assert (Sum.sum [|1.0;2.0;3.0|] = 6.0)
  
