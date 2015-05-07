module X = struct
  module Show = struct
    let int = string_of_int
  end
end

module Z = struct
  module Show = struct
    type 'a __imp__ = private 'a
    external pack : _d:'a -> 'a __imp__ = "%identity"
    (* private alias and %identity assure this wrapping is cost 0 *)
  end
end
    
open X
open Z

let show (type a) ?x:imp = match imp with
  | None -> assert false
  | Some imp -> (imp : (a -> string) Show.__imp__ :> (a -> string) ) 

let () = assert (show ~x:(assert false [@imp3]) 1 = "1")

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

let show_twice : ?x:(('a -> string) Z.Show.__imp__) -> 'a -> string =
  fun ?x v -> show v ^ show v
  
