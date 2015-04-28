(* Let's start to have some functions to show various data types 
   in one module Show *)
module Show = struct
  let int = string_of_int
  let float = string_of_float
  let list show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
end

(* It should work like this *)
let () = assert (Show.int 1 = "1")
let () = assert (Show.float 1.0 = "1.")
let () = assert (Show.(list int) [1;2] = "[ 1; 2 ]")

(* They are all `show`, let's the compiler to create the values!
   This is the idea of the value implicits.

     let () = assert ([%imp Show] 1 = "1")
     let () = assert ([%imp Show] 1.0 = "1.")
     let () = assert ([%imp Show] [1;2] = "[ 1; 2 ]")

   [%imp M] means create a value which matches with the current typing context
   using the values defined in M.
*)

(* An example of deriving value implicits *)

let show_twice imp x = imp x ^ imp x

let () = assert (show_twice Show.(list int) [1;2] = "[ 1; 2 ][ 1; 2 ]")
(*
let () = assert (show_twice [%imp Show] [1;2] = "[ 1; 2 ][ 1; 2 ]")
*)

(* Put the idea back to the first examples: *)
let show (f : 'a -> string) = f

(* Now it looks like overloading:

     let () = assert (show [%imp Show] 1 = "1")
     let () = assert (show [%imp Show] 1.0 = "1.")
     let () = assert (show [%imp Show] [1;2] = "[ 1; 2 ]")
*)

(* We can extend the recipe. Candidates are searched recursively
   into the sub modules:
*)
module Show' = struct
  module Show = Show
  let tuple imp1 imp2 (x,y) = "(" ^ show imp1 x ^ ", " ^ show imp2 y ^ ")"
end

let () = assert (show_twice (Show'.tuple Show'.Show.int Show'.Show.float) (1,1.2) = "(1, 1.2)")
(*
let () = assert (show_twice [%imp Show'] [1;2] = "[ 1; 2 ][ 1; 2 ]")
*)


(* We may be able to merge recipes at [%imp] side: *)
module Show'' = struct
  let tuple imp1 imp2 (x,y) = "(" ^ show imp1 x ^ ", " ^ show imp2 y ^ ")"
end

let () = assert (show_twice (Show''.tuple Show.int Show.float) (1,1.2) = "(1, 1.2)")
(*
let () = assert (show_twice [%imp Show Show''] [1;2] = "[ 1; 2 ][ 1; 2 ]")
*)

(* Composing modules are bit inefficient and listing more than one modules
   are bit boring. Any good idea?

   We can make use of open.  [%imp* Show]  (or some better syntax)
   seek child module Show defined in explicitly opened modules 
   in the current context..
  
   let () = assert (show_twice [%imp* Show] [1;2] = "[ 1; 2 ][ 1; 2 ]") 

   Possible problem:

     open X  (* has Show *)
     module X = struct
       ...
     end 

   Now X.Show is not accessible. ppx is a source based solution :-( 
*)


(* show and show_twice normally use [%imp Show], so writing Show is boring.
   Any good way to omit them like

   show [%imp] 1
   show_twice [%imp] 1

   ???

   This requires the shift of the information of the module name Show from
   syntax to type:  [%imp] must have a type with "Show".
   
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
