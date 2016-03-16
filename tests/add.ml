module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp_spec Add]) Ppx_implicits.t

let add : ?imp:'a add -> 'a -> 'a -> 'a = Ppx_implicits.imp

let () = assert (add 1 2 = 3)
let () = assert (add 1.2 3.4 = 4.6)
