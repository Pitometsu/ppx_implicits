(* 
   
   Let's start to have functions to show various data types in one module Show.

*)

module Show = struct
  let int = string_of_int
  let float = string_of_float
  let list ~_d:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  (* currently a label starts with '_' is required to express recipe dependencies *)
end

(*

  They work as follows. Pretty normal.

*)

let () = assert (Show.int 1 = "1")
let () = assert (Show.float 1.0 = "1.")
let () = assert (Show.(list ~_d:int) [1;2] = "[ 1; 2 ]")

(*

  Let the compiler to create show functions for given type contexts by composing
  values defined in Show.  This is the idea of the value implicits.

  [%imp M] means to create a value which matches with the current typing context
  composing the values defined in M.
*)

let () = assert ([%imp Show] 1 = "1")
let () = assert ([%imp Show] 1.0 = "1.")
let () = assert ([%imp Show] [1;2] = "[ 1; 2 ]")
  
(*

  Deriving value implicits.  You can defined higher order functions which take
  implicit values from the outer scope:

*)

let show_twice imp x = imp x ^ imp x

let () = assert (show_twice Show.(list ~_d:int) [1;2] = "[ 1; 2 ][ 1; 2 ]")

(*

  [%imp Show] can compose Show.(list int):

*)

let () = assert (show_twice [%imp Show] [1;2] = "[ 1; 2 ][ 1; 2 ]")

(*

  If we put this idea of higher order functions taking implicit values 
  back to the first show example, we get:

*)
  
let show (f : 'a -> string) = f

let () = assert (show [%imp Show] 1 = "1")
let () = assert (show [%imp Show] 1.0 = "1.")
let () = assert (show [%imp Show] [1;2] = "[ 1; 2 ]")

(*
  it looks like overloading with explicit dispatch.
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
let () = assert (show_twice [%imp Show'] [1;2] = "[ 1; 2 ][ 1; 2 ]")


(*
  
  We may also be able to list multiple recipes inside [%imp]: 

*)

module Show'' = struct
  let tuple imp1 imp2 (x,y) = "(" ^ show imp1 x ^ ", " ^ show imp2 y ^ ")"
end

let () = assert (show_twice (Show''.tuple Show.int Show.float) (1,1.2) = "(1, 1.2)(1, 1.2)")
let () = assert (show_twice [%imp Show, Show''] [1;2] = "[ 1; 2 ][ 1; 2 ]")

(*

  Easier closed extension by module open

  Composing modules are bit inefficient and listing more than one modules
  are bit boring. Any good idea?

  We can make use of open M.  [%imp2 Show]  (or some better syntax)
  seek child module Show defined in explicitly opened modules 
  in the current context.

*)

module X = struct
  module Show = Show
end

module Y = struct
  module Show = Show''
end

open X
open Y

let () = assert (show_twice [%imp2 Show] [1;2] = "[ 1; 2 ][ 1; 2 ]")

(*
  is equivalent with 
*)
  
let () = assert (show_twice [%imp X.Show, Y.Show] [1;2] = "[ 1; 2 ][ 1; 2 ]") 

(*
  Possible problem:

  open X  (* has Show *)
  module X = struct
  ...
  end 
  
  [%imp2 Show]

  Now X.Show is not accessible at the position of [%imp2 Show]. 
  ppx is a source based solution.  If a recipe module N.M for [%imp* M] is shadowed 
  it must warn or reject such shadowing.

*)


(*

  Open extension:


  show and show_twice normally use [%imp Show] or [%imp2 Show], so writing Show everytime is boring.
  It would be better if we can omit writing Show like:

  show [%imp] 1
  show_twice [%imp] 1

  This requires the shift of the information of the module name Show from
  syntax to type:  functions show and show_twice must have types which enforce
  [%imp] have types with "Show".
   
*)

module Z = struct
  module Show = struct
    type 'a __imp__ = private 'a
    external pack : _d:'a -> 'a __imp__ = "%identity"
    (* private alias and %identity assure this wrapping is cost 0 *)
  end
end
    
let show (type a) imp = let imp = (imp : a Z.Show.__imp__ :> a) in imp

let () = assert (show (Z.Show.pack ~_d:X.Show.int) 1 = "1")

(*
  X.Show.pack is actually automatically composable.
*)

open Z

let () = assert (show [%imp2 Show] 1 = "1")

(*

  We want to replace this [%imp2 Show] by [%imp3].

  We introduce a conversion from [%imp3] of type (t1, .., tn) X.Y.Z.name
  to                             [%imp2 Y]

*)
  
let () = assert (show [%imp3] 1 = "1")
let x_show_twice imp x = show imp x ^ show imp x
let () = assert (x_show_twice [%imp3] 1 = "11")

(*

  What about module alias? 

*)
module W = struct
  module Wohs = Z.Show

  let show (type a) imp = let imp = (imp : a Wohs.__imp__ :> a) in imp

(* In this case, [%imp] : t Wohs.__imp__  should become [%imp2 Show] ? 
   Or not?

   If [%imp] : t X.__imp__   where X is a functor parameter, what happens,
   probably we should reject it.
*)
end

(*
   Implicit applicaiton of [%imp] by the optional parameters

   Omitting [%imp].  Writing [%imp] is now boring, but vanilla 
   OCaml has no way of omitting code... except the optional arguments!
*)

let show (type a) ?x:imp = match imp with
  | None -> assert false
  | Some imp -> (imp : (a -> string) Show.__imp__ :> (a -> string) ) 

let () = assert (show ~x:[%imp3] 1 = "1")

(*
   
  We now introduce a rule:

  ?label:None  where None has a type X.Y.Z.__imp__, it is replaced by [%imp3].
  Now __imp__ becomes a special name.

*)
let () = assert (show ?x:None 1 = "1")

(*

  This is equivalent with

*)

let () = assert (show 1 = "1")
