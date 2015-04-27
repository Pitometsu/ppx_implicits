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

module Int = struct
  type a = int
  let show = string_of_int
end

module Float = struct
  type a = float
  let show = string_of_float
end

module List(A : Show) = struct
  module Instance = struct
    (* Need to extend the instance space with A *)
    let a : A.a show = (module A)
  end
  type a = A.a list
  (* Need type constraint so that the internal use of show can be resovled *)
  let show (xs : a) = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
end

module Instance = struct
  let int : int show = (module Int)
  let float : float show = (module Float)

  (* This is complex... We want to write module List = List ... *)
  let list (type a) ~_d:(_d: a show) : a list show =
    let module A = (val _d) in
    (module List( (val _d) ))  (* <- crazy parens! *)
end

let () = assert (show 1 = "1")
let () = assert (show 1.0 = "1.")
let () = assert (show [1;2;3] = "[ 1; 2; 3 ]") 

let show_3 (sh : ?_d: 'a show -> 'a -> string) =
  sh 3
