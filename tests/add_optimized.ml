module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp Add]) Ppx_implicits.t

val %imp add : ?d:'a add -> 'a -> 'a -> 'a

let () = assert (add 1 2 = 3)
let () = assert (add 1.2 3.4 = 4.6)

let double ?d x = add ?d x x
