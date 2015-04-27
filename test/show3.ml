module type Show = sig
  type a
  val show : a -> string
end
type 'a show = (module Show with type a = 'a)

(*
  MI : val show : (implicit S : Show) -> S.t -> string
  PT : val show : ?_d:(module Show with type a = 'a) -> 'a -> string
       or
       val show : ?_d:'a show -> 'a -> string

  The both look almost the same.
*)

let from_Some = function
  | None -> assert false
  | Some v -> v

let show (type a) ?_d = let module S = (val (from_Some _d : a show)) in S.show
(* or *)
let show1 (type a) ?_d = match _d with
  | Some ((module S) : a show) -> S.show
  | None -> assert false
(* or *)
let show2 (type a) ?_d:(Some ( (module S) : a show )) = S.show
(* or *)
let show3' (type a) ~_d:( (module S) : a show ) = S.show
let show3 ?_d = match _d with
  | None -> assert false
  | Some _d -> show3' ~_d

(*
  MI : let show (implicit S : Show) x = S.show x

  PT : We clearly need an easier way to define the entry point.
       show2 is attractive but it is still bit lousy and has a waninrg.

       We simply auto-generate the entry points.

       module type Show = sig
         type a
         val show : a -> string
       end [@@typeclass]
*)

module Int = struct
  type a = int
  let show = string_of_int
end

let int : int show = (module Int)
(* MI : implicit module Int = struct
          type a = int
          let show = string_of_int
        end

   PT : PT needs to create a first class value, since compiler-libs do not
        expose enough API functions to compare a module type 
        and a packed module type.

        We simply auto-generate the value from

        module Int = struct
          type a = int
          let show = string_of_int
        end [@@typeclass: int show]
*)

module Float = struct
  type a = float
  let show = string_of_float
end

module List(A : Show) = struct
  module Instance = struct
    (* Need to extend the instance space with A *)
    let a : A.a show = (module A)
  end
  type a = A.a list
  (* Need type constraint so that the internal use of show can be resovled *)
  let show (xs : a) = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
end

(*
  MI: implicit functor List (S: Show) = struct
        type a = S.a list
        let show (xs : a) = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
      end

      Good thing is that S is automatically integrated into the instance
      of show.

  PT: Integration of the argument is very lousy.
    module List(A : Show) = struct
      module Instance = struct
        (* Need to extend the instance space with A *)
        let a : A.a show = (module A)
      end
      type a = A.a list
      (* Need type constraint so that the internal use of show can be resovled *)
      let show (xs : a) = "[ " ^ String.concat "; " (List.map show xs) ^ " ]"
    end
*)

module Instance = struct
  let int : int show = (module Int)
  let float : float show = (module Float)

  (* PT: This is complex... This is a must to auto-generate the first order
         dictionary code *)
  let list (type a) ~_d:(_d: a show) : a list show =
    let module A = (val _d) in
    (module List( (val _d) ))  (* <- crazy parens! *)
end

let () = assert (show 1 = "1")
let () = assert (show 1.0 = "1.")
let () = assert (show [1;2;3] = "[ 1; 2; 3 ]") 

let show_3 (sh : ?_d: 'a show -> 'a -> string) =
  sh 3

let show_twice ?_d x = show ?_d x ^ show ?_d x
(*
  MI : let show_twice (implicit S : Show) (x : S.t) = show x ^ show x

  PT : let show_twice ?_d x = show ?_d x ^ show ?_d x

     Compared with MI, PT requires manual wiring of dispatching, which
     would be lousy for complex cases.

     It should be nice if the dispatched dictionary is automatically
     integrated into the instance search space:

     let show_twice (type a) ?_d:(Some (d : a show)) (x : a) = 
       let module Instance = struct
         module I = Instance
         let d = d
       end in
       show x ^ show x

     But currently it is not possible since PT cannot handle instances
     with generalized type nodes
*)

