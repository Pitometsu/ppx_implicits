module type Show = sig
  type a
  val show : a -> string
end
  
type 'a s = (module Show with type a = 'a)

type 'a show = ('a s, [%imp_spec opened Show]) Ppx_implicits.Runtime.t

let show : ?_d:'a show -> 'a -> string = fun (type a) ?_d ->
  let m : a s = Ppx_implicits.Runtime.(get (from_Some _d)) in
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
    
    let list (type a) ~_d:(_d: a s) : a list s =
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

let show_twice ?_d x = show ?_d x ^ show ?_d x
let () = assert (show_twice 1 = "11")

let show_twice ?_d:(_ : 'a show option) (x : 'a) =
  show x ^ show x

let () = assert (show_twice 1 = "11")
  
