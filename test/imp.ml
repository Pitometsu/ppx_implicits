(* 
   
   Let's start to have functions to show various data types in one module Show.

*)

module Show = struct
  let int = string_of_int
  let float = string_of_float
  let list show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
end

(*

  They work as follows. Pretty normal.

*)

let () = assert (Show.int 1 = "1")
let () = assert (Show.float 1.0 = "1.")
let () = assert (Show.(list int) [1;2] = "[ 1; 2 ]")

(*

  Let the compiler to create show functions for given type contexts by composing
  values defined in Show.  This is the idea of the value implicits.

     let () = assert ([%imp Show] 1 = "1")
     let () = assert ([%imp Show] 1.0 = "1.")
     let () = assert ([%imp Show] [1;2] = "[ 1; 2 ]")

  [%imp M] means to create a value which matches with the current typing context
  composing the values defined in M.
*)

(*

  Deriving value implicits.  You can defined higher order functions which take
  implicit values from the outer world:

*)

let show_twice imp x = imp x ^ imp x

let () = assert (show_twice Show.(list int) [1;2] = "[ 1; 2 ][ 1; 2 ]")

(*

  [%imp Show] can replace Show.(list int):

  let () = assert (show_twice [%imp Show] [1;2] = "[ 1; 2 ][ 1; 2 ]")

*)

(*

  If we put this idea of higher order functions which take implicit values back to
  the first show example, we get:

*)
  
let show (f : 'a -> string) = f

(*

  and

  let () = assert (show [%imp Show] 1 = "1")
  let () = assert (show [%imp Show] 1.0 = "1.")
  let () = assert (show [%imp Show] [1;2] = "[ 1; 2 ]")

  it looks like overloading.
*)

(*

  Closed extension.

  We can extend the recipe. Candidates are searched recursively into the sub modules:

*)
  
module Show' = struct
  module Show = Show
  let tuple imp1 imp2 (x,y) = "(" ^ show imp1 x ^ ", " ^ show imp2 y ^ ")"
end

let () = assert (show_twice (Show'.tuple Show'.Show.int Show'.Show.float) (1,1.2) = "(1, 1.2)(1, 1.2)")

(*

  let () = assert (show_twice [%imp Show'] [1;2] = "[ 1; 2 ][ 1; 2 ]")

*)


(*
  
  We may also be able to list multiple recipes inside [%imp]: 

*)

module Show'' = struct
  let tuple imp1 imp2 (x,y) = "(" ^ show imp1 x ^ ", " ^ show imp2 y ^ ")"
end

let () = assert (show_twice (Show''.tuple Show.int Show.float) (1,1.2) = "(1, 1.2)(1, 1.2)")

(*

  let () = assert (show_twice [%imp Show Show''] [1;2] = "[ 1; 2 ][ 1; 2 ]")

*)

(*

  Easier extension by module open

  Composing modules are bit inefficient and listing more than one modules
  are bit boring. Any good idea?

  We can make use of open M.  [%imp* Show]  (or some better syntax)
  seek child module Show defined in explicitly opened modules 
  in the current context.
  
  let () = assert (show_twice [%imp* Show] [1;2] = "[ 1; 2 ][ 1; 2 ]") 

  Possible problem:

  open X  (* has Show *)
  module X = struct
  ...
  end 
  
  [%imp* Show]

  Now X.Show is not accessible at the position of [%imp* Show]. 
  ppx is a source based solution.  If a recipe module N.M for [%imp* M] is shadowed 
  it must warn or reject such shadowing.

*)


(*

  Open extension:


  show and show_twice normally use [%imp Show], so writing Show is boring.
  Any good way to omit them like

  show [%imp] 1
  show_twice [%imp] 1

  This requires the shift of the information of the module name Show from
  syntax to type:  functions show and show_twice must have types which enforce
  [%imp] have types with "Show".
   
*)

module X = struct
  module Show = struct
    type 'a __imp__ = private 'a
    external pack : 'a -> 'a __imp__ = "%identity"
  end
    
  let show (type a) imp = let imp = (imp : a Show.__imp__ :> a) in imp
end

let () = assert (X.show (X.Show.pack Show.int) 1 = "1")
(* Sounds pretty wiered but we can traslate ([%imp] : t X.Show.t) to
   (X.Show.pack ([%imp* Show] : t))
*)

let x_show_twice imp x = X.show imp x ^ X.show imp x
let () = assert (x_show_twice (X.Show.pack Show.int) 1 = "11")
(* 
let () = assert (x_show_twice [%imp] 1 = "11") 
*)

(* What about module alias? *)
module Y = struct
  module Wohs = X.Show

  let show (type a) imp = let imp = (imp : a Wohs.__imp__ :> a) in imp

(* In this case, [%imp] : t Wohs.__imp__  should become [%imp* Show] ? 
   Or not?

   If [%imp] : t X.__imp__   where X is a functor parameter, what happens,
   probably we should reject it.
*)
end

(* Omitting [%imp].  Writing [%imp] is now boring, but vanilla 
   OCaml has no way of omitting code... except the optional arguments!
*)

module Z = struct
  module Show = X.Show
  let show (type a) ?x:imp = 
    match (imp : (a -> string) Show.__imp__ option :> (a -> string) option) with
    | None -> assert false
    | Some show -> show
end

let () = assert (Z.show ~x:(Z.Show.pack Show.int) 1 = "1")
(*
let () = assert (Z.show ?x:(Some [%imp]) 1 = "1")
*)

(* We need to a special rule (None : t Z.Show.__imp__ option)  => (Some [%imp]).

   But how to distinguish Z.Show.__imp__ from the other normal types?
   The name "__imp__"!
*)
