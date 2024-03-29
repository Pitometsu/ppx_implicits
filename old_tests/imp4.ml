module Z = struct
  module Show = struct
    module type Show = sig
      type a
      val show : a -> string
    end
  
    type 'a t = (module Show with type a = 'a)
  end
end

module M = struct
  module Show = struct
    type 'a t = private 'a Z.Show.t
    [%%imp_spec opened Show]
    external pack' : 'a Z.Show.t -> 'a t = "%identity"
    let pack ~_x = Some (pack' _x)
  end
end

open M
  
let show (type a) ?_imp = match _imp with
  | None -> assert false
  | Some imp -> let module D = (val (imp : a M.Show.t :> a Z.Show.t) ) in D.show

module X = struct
  module Show = struct
    let int : int Z.Show.t =
      let module Int = struct
        type a = int
        let show = string_of_int
      end in (module Int)
    
    let float : float Z.Show.t = 
      let module Float = struct
        type a = float
        let show = string_of_float
      end in (module Float)
    
    let list (type a) ~_d:(_d: a Z.Show.t) : a list Z.Show.t =
      let module List = struct
        type a' = a list
        type a = a'
        (* Need type constraint so that the internal use of Show.show can be resovled *)
        let show (xs : a) = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
      end in (module List)
  end
end

open X

let () = assert (show 1 = "1")
let () = assert (show 1.0 = "1.")
let () = assert (show [1;2;3] = "[ 1; 2; 3 ]") 
let () = assert (show [[1]; [2;3]; [4;5;6]] = "[ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ]")

let show_twice ?_imp x = show ?_imp x ^ show ?_imp x
let () = assert (show_twice 1 = "11")

let show_twice ?_imp:(i : 'a M.Show.t option) (x : 'a) =
  show x ^ show x

let () = assert (show_twice 1 = "11")
