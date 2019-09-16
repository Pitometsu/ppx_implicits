(* How to mimic type classes in value implicits.

   Value implicits autogenerate values of given type.
   To build a module of given context, we build a first class module value.
*)

module Show = struct

  module type S = sig
    type a
    val show : a -> string
  end

  (* type of the module *)    
  type 'a m = (module S with type a = 'a)

  (* type of the dispatched dictionary 

     We need this packing since the above m is an alias.

     (We may be able to track the aliase and get (module S with type a = 'a)
      then get the correct imp_spec since S is defined in Show.
      But it is a future work.)
  *)      
  type 'a t = Packed of (module S with type a = 'a)
      
  [%%imp_spec opened ShowInstance]

  let unpack (Packed x) = x
  let unpack_opt = function None -> assert false | Some (Packed x) -> x

  (* Default instances *)
  module Instances = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end

  let show (type a) ?_imp =
    let module M = (val (unpack_opt _imp : a m)) in
    M.show

end


module ShowInt = struct
  module ShowInstance = struct

    let int : int Show.m = 
      let module M = struct
        type a = int
        let show  = string_of_int
      end in
      (module M)

  end
end

(* #1, Example of explicit dispatch code *)
module ShowList = struct
  module ShowInstance = struct

    let list (type a) ~_x:(_x : a Show.m) : a list Show.m = 
      let module M(X : Show.S) = struct
        type a = X.a list
        let show xs = "[ " ^ String.concat "; " (List.map X.show xs) ^ " ]"
      end in
      let module Y = M( (val _x) ) in
      (module Y)

  end
end

(* #2, Slightly simple version w/o a functor *)
module ShowTwin = struct
  module ShowInstance = struct

    let tuple (type b) ~_x:(_x : b Show.m) : (b * b) Show.m = 
      let _imp = Show.Instances.pack ~_x in 
      let module M = struct
        type a = b * b
        let show (x,y) = "( " ^ Show.show ~_imp x ^ ", " ^ Show.show ~_imp y ^ " )"
      end in
      (module M)

  end
end

(* #3, Let ppx_implicits wire the dispatch code automatically *)
module ShowTriple = struct
  module ShowInstance = struct

    let tuple (type b) ~_x:(_x : b Show.m) : (b * b * b) Show.m = 

      let module M = struct
        type a = b * b * b
        (* Tricky. We need (b * b * b) or the types of x, y and z become unrelated with b *)            
        let show (x,y,z : b * b * b) = "( " ^ Show.show x ^ ", " ^ Show.show y ^ ", " ^ Show.show z ^ " )"
      end in
      (module M)

  end
end

open ShowInt
open ShowList
open ShowTwin
open ShowTriple
 
let () = assert (Show.show [1;2] = "[ 1; 2 ]")
let () = assert (Show.show (1,2) = "( 1, 2 )")
let () = assert (Show.show (1,2,3) = "( 1, 2, 3 )")
