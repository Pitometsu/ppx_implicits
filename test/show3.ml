module type Show = sig
  type a
  val show : a -> string
end
type 'a show = (module Show with type a = 'a)

(*
val show : (implicit S : Show) -> S.t -> string

val show : ?_d:'a show -> 'a -> string

let show (implicit S : Show) x = S.show x

let show ?_d:(module S : Show) x = S.show x
*)
let from_Some = function
  | None -> assert false
  | Some v -> v

let show (type a) ?_d = 
  let module S = (val (from_Some _d : a show)) in S.show

let int : int show =
  let module Int = struct
    type a = int
    let show = string_of_int
  end in
  (module Int)

let float : float show =
  let module Float = struct
    type a = float
    let show = string_of_float
  end in
  (module Float)

let list (type a) ~_d:(_d : a show) : a list show =
  let module Instance = struct
    let _d = _d
  end in
  let module A = (val _d) in
  let module List = struct
    type a = A.a list
    let show (xs : a) = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  end in
  (module List)
    
module Instance = struct
  let int = int
  let float = float
  let list = list
end

let () = assert (show 1 = "1")
let () = assert (show 1.0 = "1.")
let () = assert (show [1;2;3] = "[ 1; 2; 3 ]") 

let show_3 (sh : ?_d: 'a show -> 'a -> string) =
  sh 3

  
