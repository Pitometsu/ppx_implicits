module Show = struct
  let int = string_of_int
  let float = string_of_float
end

let () = assert ([%imp Show] 1 = "1")   
(* [%imp Show] is expanded to Show.int *)

let () = assert ([%imp Show] 1.0 = "1.")
(* [%imp Show] is expanded to Show.float *)

module Show2 = struct
  include Show

  let list ~_d:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  (* currently a label starts with '_' is required to express instance dependencies *)
end

let () = assert ([%imp Show2] [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ] = "[ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ]")
(* [%imp Show] is expanded to Show2.(list ~_d:(list ~_d: int)) *)
  
module Show3 = struct
  let list = Show2.list
end

let () = assert ([%imp Show, Show3] [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ] = "[ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ]")
(* [%imp Show] is expanded to Show3.list ~_d:(Show3.list ~_d: Show.int) *)

module MInt = struct
  module Show = struct
    let int = string_of_int
  end
end

module MFloat = struct
  module Show = struct
    let float = string_of_float
  end
end

module MList = struct
  module Show = struct
    let list ~_d:show xs = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
  end
end

open MInt
open MFloat
open MList
     
let () = assert ([%imp opened Show] [ 1 ] = "[ 1 ]")
(* Here, [%imp opened Show] is equivalent with [%imp MInt.Show, MFloat.Show, MList.Show] *)

let show imp x = imp x

let () = assert (show [%imp opened Show] 1.2 = "1.2")
let () = assert (show [%imp opened Show] [ 1 ] = "[ 1 ]")


module Implicit_parameters = struct

  module Show = struct
    type 'a __imp__ = Packed of 'a
    [%%imp_policy opened ShowInstance]

    let unpack = function None -> assert false | Some (Packed x) -> x  
    let show ?_imp x = unpack _imp x
  end

  module ShowBase = struct
    module ShowInstance = struct
      let pack ~_d = Show.Packed _d
      let pack_opt ~_d = Some (pack ~_d)
    end
  end

  module Int = struct
    module ShowInstance = struct
      let int = string_of_int
    end
  end

  open ShowBase (* to make use of ShowBase.ShowInstance.pack as an instance *)
  open Int      (* to make use of Int.ShowInstance.int as an instance *)

  let ()  = assert (Show.show 1 = "1")
  (* is a sugar of              Show.show ?imp:None 1
     is replaced by             Show.show ~imp:[%imp] 1
     which is equivalent with   Show.show ~imp:[%imp opened ShowInstance] 1
     which is equivalent with   Show.show ~imp:[%imp ShowBase.ShowInstance, Int.ShowInstance] 1
     and is finally expanded to Show.show ~imp:(ShowBase.ShowInstance.pack ~_d:Int.ShowInstance.int) 1
  *)

  let show_twice ?_imp x = Show.show ?_imp x ^ Show.show ?_imp x
  
  let () = assert (show_twice 1 = "11")
end
  
