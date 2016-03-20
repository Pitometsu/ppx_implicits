(* length : 
   to see length : 'a list -> int works correctly
*)
type 'a length = ('a -> int, [%imp_spec Length]) Ppx_implicits.t
    
let length : ?d:'a length -> 'a -> int = Ppx_implicits.imp

module Length = struct
  let string = String.length
  let list = List.length
  let array = Array.length
end

let () = assert (length "hello" = 5)
let () = assert (length [1;2;3] = 3)
let () = assert (length [|1;2;3|] = 3)
