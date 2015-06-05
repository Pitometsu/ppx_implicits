(*

  Tools

*)

(* (@@) is too strong *)
external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"

let (!!%) = Format.eprintf

let flip f x y = f y x
let flip2 f x y z = f z x y
    
module Option = struct
  let map f = function
    | None -> None
    | Some v -> Some (f v)
end

module List = struct
  include List

  let rec filter_map f = function
    | [] -> []
    | x :: xs -> match f x with
      | None -> filter_map f xs
      | Some y -> y :: filter_map f xs

  let concat_map f xs = concat (map f xs)
end 

module String = struct
  include String
  let is_prefix p s = try sub s 0 (length p) = p with _ -> false
end

module Format = struct
  include Format

  let ksprintf f fmt =
    let buf = Buffer.create 100 in
    let ppf = formatter_of_buffer buf in
    kfprintf (fun ppf -> pp_print_flush ppf (); f (Buffer.contents buf)) ppf fmt
end

let errorf fmt = Format.ksprintf (fun s -> prerr_endline s; exit 1) fmt

let protect f = try `Ok (f ()) with e -> `Error e
let unprotect = function
  | `Ok v -> v
  | `Error e -> raise e

let warn f = 
  Format.eprintf "@[<2>Warning:@ ";
  f ();
  Format.eprintf "@]@.";
