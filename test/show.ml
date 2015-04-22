module Show = struct
  module type Show = sig
    type a
    val show : a -> string
  end

  type 'a t = (module Show with type a = 'a)

  let show (type a) ?_d = match _d with
    | None -> assert false
    | Some _d ->
        let module D = (val (_d : a t)) in D.show
end

module Int = struct
  type a = int
  let show = string_of_int
end

module Float = struct
  type a = float
  let show = string_of_float
end

module List(A : Show.Show) = struct
  module Instance = struct
    (* Need to extend the instance space with A *)
    let a : A.a Show.t = (module A)
  end
  type a = A.a list
  (* Need type constraint so that the internal use of Show.show can be resovled *)
  let show (xs : a) = "[ " ^ String.concat "; " (List.map Show.show xs) ^ " ]"
end

module Instance = struct
  let int : int Show.t = (module Int)
  let float : float Show.t = (module Float)

  (* This is complex... We want to write module List = List ... *)
  let list (type a) ~_d:(_d: a Show.t) : a list Show.t =
    let module A = (val _d) in
    (module List( (val _d) ))  (* <- crazy parens! *)
end


let () = assert (Show.show 1 = "1")
let () = assert (Show.show 1.0 = "1.")
let () = assert (Show.show [1;2;3] = "[ 1; 2; 3 ]") 

  
  
