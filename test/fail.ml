module Show = struct
  module type Show = sig
    type a
    val show : a -> string
  end

  type 'a t = (module Show with type a = 'a)

  let show (type a) ?_d = match _d with
    | None -> assert false
    | Some _d -> let module D = (val (_d : a t)) in D.show
end

module Int = struct
  type a = int
  let show = string_of_int
end

module ListInt = struct
  type a = int list
  let show xs = String.concat " " (List.map Int.show xs)
end

module Instance = struct
  let int : int Show.t = (module Int)
  let listint : int list Show.t = (module ListInt)
end

(* This must fail, since [] is 'a list.
   Instantiating with Instnace.listint is changes the type!
*)
let f (x : 'a list) = print_string @@ Show.show x; x

(* Wrong overload resolution above changes the type of f
   from 'a list -> 'a list to int list -> int list
   which makes the following code ill-typed!
*)
let () = assert (f ["hello"] = ["hello"])
