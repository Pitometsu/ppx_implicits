module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp Add]) Ppx_implicits.t

let add : ?d:'a add -> 'a -> 'a -> 'a = Ppx_implicits.imp

let () = assert (add 1 2 = 3)
let () = assert (add 1.2 3.4 = 4.6)

let double ?d x = add ?d x x

let double ?d x = (Ppx_implicits.imp : ?d:'a add -> 'a -> 'a -> 'a) ?d x x
let () = assert (double 2 = 4)
  
