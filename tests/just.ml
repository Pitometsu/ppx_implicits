module Show = struct
  type 'a __imp__ = Packed of ('a -> string)
  (* This takes only the direct members as instances.
     Values inside sub-modules are ignored.
  *)
  [%%imp_policy Just Show] 
  let pack ~_x = Some (Packed _x)
  let unpack = function
    | None -> assert false
    | Some (Packed x) -> x
  let show ?imp = unpack imp
  module Sub = struct
    let int (x:int) = "hello" (* this should not be chosen *)
  end
  let int = string_of_int
end

let () = assert ( Show.show 1 = "1" )
