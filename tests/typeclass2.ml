module Show = struct

  module type S = sig
    type a
    val show : a -> string
  end

  type 'a s = (module S with type a = 'a)

  include Ppx_implicits.Implicits.Make(struct
    type 'a t = 'a s
  end)

  [%%imp_spec opened ShowInstance]

  let show (type a) ?_imp = let module M = (val (unpack_opt ?_imp : a s)) in M.show
end


module Int = struct
  module ShowInstance = struct

    let int : int Show.s = 
      let module M = struct
        type a = int
        let show  = string_of_int
      end in
      (module M)

  end
end

(* Example of explicit dispatch code *)
module List' = struct
  module ShowInstance = struct

    let list (type a) ~_x:(_x : a Show.s) : a list Show.s = 
      let module M(X : Show.S) = struct
        type a = X.a list
        let show xs = "[ " ^ String.concat "; " (List.map X.show xs) ^ " ]"
      end in
      let module Y = M( (val _x) ) in
      (module Y)

  end
end

(* Slightly simple version w/o a functor *)
module Twin = struct
  module ShowInstance = struct

    let tuple (type b) ~_x:(_x : b Show.s) : (b * b) Show.s = 
      let _imp = Show.Instances.pack ~_x in 
      let module M = struct
        type a = b * b
        let show (x,y) = "( " ^ Show.show ~_imp x ^ ", " ^ Show.show ~_imp y ^ " )"
      end in
      (module M)

  end
end

(* Let ppx_implicits wire the dispatch code automatically *)
module Triple = struct
  module ShowInstance = struct

    let tuple (type b) ~_x:(_x : b Show.s) : (b * b * b) Show.s = 

      let module M = struct
        type a = b * b * b
        (* Tricky. We need (b * b * b) or the types of x, y and z become unrelated with b *)            
        let show (x,y,z : b * b * b) = "( " ^ Show.show x ^ ", " ^ Show.show y ^ ", " ^ Show.show z ^ " )"
      end in
      (module M)

  end
end

open Show
open Int
open List'
open Twin
open Triple
 
let () = assert (Show.show [1;2] = "[ 1; 2 ]")
let () = assert (Show.show (1,2) = "( 1, 2 )")
let () = assert (Show.show (1,2,3) = "( 1, 2, 3 )")
