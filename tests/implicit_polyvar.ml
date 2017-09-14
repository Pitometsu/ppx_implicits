(* compile with ppx_implicits ab783d180c35 (0.3.1) *)
(* ocamlfind ocamlc -c -package ppx_implicits implicit_polyvar.ml *)

module Read = struct
  type 'a t = (string -> 'a, [%imp Readers]) Ppx_implicits.t
  let unpack : 'a t -> string -> 'a = fun d -> Ppx_implicits.imp ~d
end

let read ?(_reader:'a Read.t option) = match _reader with Some x -> Read.unpack x | None -> failwith "no instance"

module M = struct
  module Readers = struct
    let _a : string -> [`A] = function "`A" -> `A | s -> failwith ("can't parse as `A:" ^s)
  end
end

open M


let _ =        
  read "`A" |> function
  | `A -> print_endline "A"
  | `B -> print_endline "B"

(* File "implicit_polyvar.ml", line 23, characters 4-6: *)
(* Error: This pattern matches values of type [? `B ] *)
(*        but a pattern was expected which matches values of type [ `A ] *)
(*        The second variant type does not allow tag(s) `B *)
