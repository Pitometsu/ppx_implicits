(* Demonstration of [%%imp_policy], instance search space policy control. *) 

module Show = struct
  let int = string_of_int
  let float = string_of_float
end

module Show2 = struct
  let list ~_x:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
end
  
module ShowImp = struct
  type 'a __imp__ = Packed of ('a -> string)

  [%%imp_policy ShowImp]
  (* [%%imp_policy] instructs how to collect the instances for [t __imp__].

     [%%imp_policy ShowImp] means the values defined under a module accessible
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

