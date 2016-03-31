module type Length = sig
  type a
  val length : a -> int
end [@@typeclass]

(* Ideal syntax:

   typeclass 'a Length = sig
     val length : 'a -> int
   end
*)

module LengthString = struct
  let length = String.length
end [@@instance: (module Length with type a = string)]
  
module LengthList = struct
  let length = List.length
end [@@instance: (module Length with type a = [%var:'a] list)]
(* It would be nicer if we could write simply 

     [module Length with type a = 'a list],

   but free type variable extraction in the level of core_type seems to be
   hard and not rewarding.
*)   

(* Ideal syntax:

   instance string Length = struct
     let length = String.length
   end   

   instance 'a list Length = struct
     let length = List.length
   end
*)

let () =
  assert (Length.length "hello" = 5);
  assert (Length.length [1;2;3] = 3)

