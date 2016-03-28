module type Length = sig
  type a
  val length : a -> int
end [@@typeclass]

module LengthString = struct
  type a = string
  let length = String.length
end [@@instance Length]
  
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
    
  (*
  module type Length = sig
    type a
    val length : a -> int
  end [@@typeclass: 'a Length._class]
  *)

  module LengthList = struct
    (* no definition of type a *)
    let length = List.length
  end (* [@@instance: 'a list Length._class] *)
  
  module LengthListInstance = struct
    let dict (type alpha) : alpha list Length._module = (module (struct type a = alpha list include LengthList end))
    type __imp_instance_of__ = Length.__class__
  end

  let () =
    assert (Length.length "hello" = 5);
    assert (Length.length [1;2;3] = 3)

end

