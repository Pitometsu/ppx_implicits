(* auto conversion of [%derive.xxx: _] => [%derive.xxx: ty] *)

let () = assert @@ [%%imp deriving [%derive.show: _]] 42 = "42"
(* should expand to 
     [%derive.show: int] 42 = "42"
*)

module ShowClass : sig
  type 'a t = private 'a -> string
  [%%imp_spec deriving [%derive.show: _]]
end = struct
  type 'a t = 'a -> string
  [%%imp_spec deriving [%derive.show: _]]
end

let show ?_d = match _d with
  | None -> assert false
  | Some f -> f
