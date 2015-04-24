(* Things can be less organized than show.ml *)

(*
  ocamlc -ppx ../ppx/ppx_typeclass -o show show2.ml

  or

  (after installation)
  ocamlfind ocamlc -package ppx_typeclass -o show show2.ml
*)

module type Show = sig
  type a
  val show : a -> string
end

type 'a show = (module Show with type a = 'a)

let show (type a) ?_d = match _d with
  | None -> assert false
  | Some _d -> let module D = (val (_d : a show)) in D.show

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

  
  
