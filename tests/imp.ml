(* 
   
   Let's start to have functions to show various data types in one module Show.

*)

module Show = struct
  let int = string_of_int
  let float = string_of_float
  let list ~_d:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  (* A label starts with '_' is a constraint label,
     which is like => arrow of Haskell.
   *)
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

  [%imp M] is an expression automatically composed from the values of moduel M
  to match the current typing context.

  Actually [%imp M] is expanded to (assert false)[@imp M] so that the vanilla
  OCaml type checker can give a typing.
*)

let () = assert ([%imp Show] 1 = "1")
let () = assert ([%imp Show] 1.0 = "1.")
let () = assert ([%imp Show] [1;2] = "[ 1; 2 ]")

(*

  Inherit value implicits by let.  
  You can defined higher order functions which take implicit values 
  from the outer scope:

*)

let show_twice imp x = imp x ^ imp x

let () = assert (show_twice Show.(list ~_d:int) [1;2] = "[ 1; 2 ][ 1; 2 ]")

(*

  [%imp Show] can compose Show.(list ~d:int):

*)

let () = assert (show_twice [%imp Show] [1;2] = "[ 1; 2 ][ 1; 2 ]")

(*

  If we put this idea of higher order functions for implicit values 
  back to the first show example, we get:

*)
  
let show (f : 'a -> string) = f  
(* ['a -> string]  is the most general anti-unifier of the instances *)

let () = assert (show [%imp Show] 1 = "1")
let () = assert (show [%imp Show] 1.0 = "1.")
let () = assert (show [%imp Show] [1;2] = "[ 1; 2 ]")

(*

  it looks like overloading... but still with explicit dispatch [%imp Show].

*)

(*

  Instance search space extension.

  We can extend the instance space. At [%imp M], candidates are searched 
  recursively into the sub-modules of M:

*)
  
module Show' = struct
  module Show = Show
  let tuple ~_d1:imp1 ~_d2:imp2 (x,y) = "(" ^ show imp1 x ^ ", " ^ show imp2 y ^ ")"
end

let () = assert (show_twice (Show'.tuple ~_d1:Show'.Show.int ~_d2:Show'.Show.float) (1,1.2) = "(1, 1.2)(1, 1.2)")
let () = assert (show_twice [%imp Show'] (1,1.2) = "(1, 1.2)(1, 1.2)")


(*
  
  We may also be able to list multiple recipes inside [%imp]: 

*)

module Show'' = struct
  let tuple imp1 imp2 (x,y) = "(" ^ show imp1 x ^ ", " ^ show imp2 y ^ ")"
end

let () = assert (show_twice (Show''.tuple Show.int Show.float) (1,1.2) = "(1, 1.2)(1, 1.2)")
let () = assert (show_twice [%imp Show, Show''] [1;2] = "[ 1; 2 ][ 1; 2 ]")

(*

  Easier instance space extension by module open

  Composing modules by include and module aliases are bit inefficient 
  and bit boring. Any good idea to facilitate this?

  Haskell uses module import to define the search space for type class instances
  and we can borrow the idea: make use of open M.  [%imp opened Show]  
  seeks child module Show defined in explicitly opened modules in the current context: if we have <open X> and X.Show exists,
  X.Show is searched for [%imp opened Show].

*)

module X = struct
  module Show = Show
end

module Y = struct
  module Show = Show''
end

open X
open Y

let () = assert (show_twice [%imp opened Show] [1;2] = "[ 1; 2 ][ 1; 2 ]")

(*

  is equivalent with 

*)
  
let () = assert (show_twice [%imp X.Show, Y.Show] [1;2] = "[ 1; 2 ][ 1; 2 ]") 

(*

  Instance space by type:

  show and show_twice normally use [%imp Show] or [%imp opened Show], so writing 
  Show everytime is boring. It would be better if we can omit writing Show like:

  show [%imp] 1
  show_twice [%imp] 1

  This requires the shift of the information of the search space name Show from
  syntax to type:  functions show and show_twice must have types which enforce
  [%imp] have types with "Show".
   
*)

module Z = struct
  module Show = struct
    [%%imp_spec opened Show]        
    type 'a t = Packed of ('a -> string)
    let pack ~_d = Packed _d
    let pack_opt ~_d = Some (pack _d)
  end
end
    
let show (Z.Show.Packed f) = f

let () = assert (show (Z.Show.pack ~_d:X.Show.int) 1 = "1")

(*
  Z.Show.pack ~_d:X.Show.int is actually automatically composable.
*)

open Z

let () = assert ([%imp opened Show] 1 = "1")
let () = assert (show [%imp opened Show] 1 = "1")

(*

  We want to replace this [%imp opened Show] by [%imp].

  We introduce a conversion from [%imp] of type (t1, .., tn) X.Y.Z.t
  to                             [%imp spec]
  where spec is defined in the module X.Y.Z by [%%imp_spec spec].

*)
  
let () = assert (show [%imp] 1 = "1")
(* 

is equilvanet with the following, since [%imp] has the type Z.Show.t
and Z.Show has a declaration [%%imp_spec opened Show].

*)
let () = assert (show [%imp opened Show] 1 = "1")

(* It also works with implicit inheritance: *)
  
let x_show_twice imp x = show imp x ^ show imp x
let () = assert (x_show_twice [%imp] 1 = "11")
(*

is equivalent with

*)
let () = assert (x_show_twice [%imp opened Show] 1 = "11")
  


(*

   Implicit applicaiton of [%imp] by the optional parameters.

   Goal: omitting [%imp].  Writing [%imp] is now boring, but which 
   vanilla OCaml functionality permits us omitting some code... 
   the optional arguments!

*)

let show ?_x:imp = match imp with
  | None -> assert false
  | Some (Z.Show.Packed f) -> f

let () = assert (show ~_x:[%imp] 1 = "1")

(*
   
  We now introduce a rule:

  ?_label:None  is replaced by  ?_label:(Some [%imp]):

*)

let () = assert (show 1 = "1")
let () = assert (show ?_x:None 1 = "1")

(*

  The above two are now equivalent with

*)

let () = assert (show ?_x:(Some [%imp]) 1 = "1")

