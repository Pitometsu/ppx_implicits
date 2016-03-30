module type Length = sig
  type a
  val length : a -> int
end [@@typeclass] (* [@@typeclass: a Length._class] *)

(* Ideal syntax:

   typeclass 'a Length = sig
     val length : 'a -> int
   end
*)

module LengthString = struct
  let length = String.length
end [@@instance: (module Length with type a = string)] (* [@@instance: string Length._class] *) 
  
(* Ideal syntax:

   instance string Length = struct
     let length = String.length
   end   

   instance 'a list Length = struct
     let length = List.length
   end
*)

(*
module LengthList = struct
  type a = 'a list (* no it is not possible! *)
  let length = List.length
end [@@instance Length]
*)

module Solution1 = struct

  module LengthListInstance = struct
    let dict (type alpha) : alpha list Length._module = (module struct type a = alpha list let length = List.length end)
    type __imp_instance_of__ = Length.__class__
  end
      
  let () =
    assert (Length.length "hello" = 5);
    assert (Length.length [1;2;3] = 3)

end
  
module Solution2 = struct  

  module LengthList(A : sig type alpha end) = struct
    type a = A.alpha list
    let length = List.length
  end

  module LengthListInstance = struct
    (* Oh no! Value polymoprhism requires an eta expansion... *)
    let dict (type aa) () : aa list Length._module = (module LengthList(struct type alpha = aa end))
    type __imp_instance_of__ = Length.__class__
  end

(*
  let () =
    assert (Length.length "hello" = 5);
    assert (Length.length [1;2;3] = 3)
*)
    
end
    
module Solution3 = struct

  (* This is the most promising *)
    
  module LengthList = struct
    (* no definition of type a *)
    let length = List.length
  end
  
  module LengthListInstance = struct
    let dict (type alpha) : alpha list Length._module = (module (struct type a = alpha list include LengthList end))
    type __imp_instance_of__ = Length.__class__
  end

  let () =
    assert (Length.length "hello" = 5);
    assert (Length.length [1;2;3] = 3)

end

module FinalSolutionIdea = struct

  (* This is the most promising *)
    
  module LengthList = struct
    (* no definition of type a *)
    let length = List.length
  end (* [@@instance: Length with type a = 'a list *)
  
  module LengthListInstance = struct
    let dict (type alpha) : alpha list Length._module = (module (struct type a = alpha list include LengthList end))
    type __imp_instance_of__ = Length.__class__
  end

  let () =
    assert (Length.length "hello" = 5);
    assert (Length.length [1;2;3] = 3)

end

