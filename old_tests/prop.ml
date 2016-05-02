(* Demonstration of [%%imp_spec], instance search space spec control. *) 

module Show = struct
  let int = string_of_int
  let float = string_of_float
end

module Show2 = struct
  let list ~_x:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
end
  
module ShowImp = struct
  type 'a t = Packed of ('a -> string)

  [%%imp_spec ShowImp]
  (* [%%imp_spec] instructs how to collect the instances for [t t].

     [%%imp_spec ShowImp] means the values defined under a module accessible
     as ShowImp are used.
  *)

  let pack ~_x = Some (Packed _x)
  let unpack = function
    | None -> assert false (* overloading was not resolved *)
    | Some (Packed x) -> x
  include Show
  include Show2
end

let show ?_imp x = ShowImp.unpack _imp x

let () = assert ( show 1 = "1" )

