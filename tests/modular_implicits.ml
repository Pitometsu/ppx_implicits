module type Show = sig
  type a
  val show : a -> string
end

let from_Some = function None  -> assert false | Some x -> x

let show (type a) ?_imp =
  let module M = struct
    type 'a _t = (module Show with type a = 'a)
    include (val (from_Some _imp : a _t))
  end in
  M.show

(* Using untyped ppx the above can be sugared as:

let show [%imp M : 'a show] = M.show;;
                   
*)
