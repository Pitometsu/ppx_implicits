module Show = struct
  let int = string_of_int
  let float = string_of_float
end

module Show2 = struct
  let list ~_x:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
end
  
module ShowImp = struct
  type 'a __imp__ = Packed of ('a -> string)
  let pack ~_x = Some (Packed _x)
  let unpack = function
    | None -> assert false (* overloading was not resolved *)
    | Some (Packed x) -> x
  include Show
  include Show2
end

let show ?imp x = ShowImp.unpack imp x

let () = assert ( show 1 = "1" )
