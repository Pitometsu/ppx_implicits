(* Dispatching first class module values for type-classes *)

module type Show = sig
  type a
  val show : a -> string
end
  
type 'a s = (module Show with type a = 'a)

type 'a show = ('a s, [%imp_spec opened Show]) Ppx_implicits.t

let show : ?imp:'a show -> 'a -> string = fun (type a) ?imp ->
  (* CR jfuruse: bad... imp and ?imp collides! *)
  let impp = imp in
  let m : a s = Ppx_implicits.(get (from_Some impp)) in
  let module M = (val m) in
  M.show
  
module X = struct
  module Show = struct
    let int : int s =
      let module Int = struct
        type a = int
        let show = string_of_int
      end in (module Int)
    
    let float : float s = 
      let module Float = struct
        type a = float
        let show = string_of_float
      end in (module Float)
    
    let list (type a) ?imp:(_d : a s option) : a list s =
      (* [from_Some _d] must be usable as a s *)
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

let show_twice ?imp x = show ?imp x ^ show ?imp x
let () = assert (show_twice 1 = "11")

let show_twice ?imp:(_ : 'a show option) (x : 'a) =
  show x ^ show x

let () = assert (show_twice 1 = "11")
  
