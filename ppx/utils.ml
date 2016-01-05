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

  open Format
  let format f ppf = function
    | None -> pp_print_string ppf "None"
    | Some v -> fprintf ppf "@[<2>Some@ (@[%a@])@]" f v
end

module List = struct
  include List

  let rec filter_map f = function
    | [] -> []
    | x :: xs -> match f x with
      | None -> filter_map f xs
      | Some y -> y :: filter_map f xs

  let concat_map f xs = concat (map f xs)

  let assoc_opt x xs = try Some (assoc x xs) with _ -> None

  let partition_map f xs =
    let rec part left right = function
      | [] -> rev left, rev right
      | x::xs ->
          match f x with
          | `Left v -> part (v::left) right xs
          | `Right v -> part left (v::right) xs
    in
    part [] [] xs

  open Format   
  let rec format (sep : (unit, formatter, unit) format)  f ppf = function
    | [] -> ()
    | [x] -> f ppf x
    | x::xs -> 
        fprintf ppf "@[%a@]%t%a" 
  	f x
  	(fun ppf -> fprintf ppf sep)
  	(format sep f) xs

  let from_to f t =
    (* CR jfuruse: we should build from 'to' *)
    let rec from_to st f t =
      if f > t then rev st
      else from_to (f::st) (f+1) t
    in
    from_to [] f t
end 

module String = struct
  include String
  let is_prefix p s = try sub s 0 (length p) = p with _ -> false
end

module Hashtbl = struct
  include Hashtbl
  let to_list tbl = Hashtbl.fold (fun k v st -> (k,v) :: st) tbl []
end
  
module Format = struct
  include Format

  let ksprintf f fmt =
    let buf = Buffer.create 100 in
    let ppf = formatter_of_buffer buf in
    kfprintf (fun ppf -> pp_print_flush ppf (); f (Buffer.contents buf)) ppf fmt
end

let protect f = try `Ok (f ()) with e -> `Error e

let unprotect = function
  | `Ok v -> v
  | `Error e -> raise e

let errorf fmt = Format.ksprintf (fun s -> prerr_endline s; exit 2) fmt

let warn f = 
  Format.eprintf "@[<2>Warning:@ ";
  f ();
  Format.eprintf "@]@.";
  
