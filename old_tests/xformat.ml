(* implicit format!! *)

open Format
  
module Base = struct
  let int ppf = fprintf ppf "%d"
  let float ppf = fprintf ppf "%f"

  let list ~_imp:f ppf = 
    let rec aux ppf = function
      | [] -> ()
      | [x] -> f ppf x
      | x::xs -> 
          fprintf ppf "@[%a@];@ %a" 
            f x
            aux xs
    in
    fprintf ppf "[ @[%a@] ]" aux

  let option ~_imp:f ppf = function
    | None -> pp_print_string ppf "None"
    | Some v -> fprintf ppf "@[<2>Some@ (@[%a@])@]" f v
end

module M = struct
  type 'a t = Packed of (formatter -> 'a -> unit)
  [%%imp_spec Base]
  (* default instances *)
  module Instances = struct
    let pack ~_x = Packed _x
    let pack_opt ~_x = Some (Packed _x)
  end
  let unpack_opt = function
    | None -> assert false
    | Some (Packed x) -> x
end

let format ?_x = M.unpack_opt _x

let () =
  format std_formatter 1;
  format std_formatter 1.2;
  format std_formatter [1;2;3];

  (* explicit optional argument elimination... so lousy... *) 
  fprintf std_formatter "%a %a %a@."
    (fun ppf -> format ppf) 1
    (fun ppf -> format ppf) 1.2
    (fun ppf -> format ppf) [1;2;3];

  (* optional argument eliminator! *)
  let (!?) f x = f x in
      
  (* slightly better... *)
  fprintf std_formatter "%a %a %a@."
    !?format 1
    !?format 1.2
    !?format [1;2;3];

  (* how about using %t? Oh, ppf... *)  
  fprintf std_formatter "%t %t %t@."
    (fun ppf -> format ppf 1)
    (fun ppf -> format ppf 1.2)
    (fun ppf -> format ppf [1;2;3]);

  (* flip eliminates optional argument then flip! *)
  let flip f x y = f y x in
  
  fprintf std_formatter "%t %t %t@."
    (flip format 1)
    (flip format 1.2)
    (flip format [1;2;3])

let format' : ?_x:_ -> 'a -> formatter -> unit = fun ?_x a ppf -> format ?_x ppf a
  
let () =
  fprintf std_formatter "%t %t %t@."
    (format' 1)
    (format' 1.2)
    (format' [1;2;3])
  
let derived ?_x:(_ : 'a M.t option) (x : 'a) =
  fprintf std_formatter "%t %t %t@."
    (format' 1)
    (format' 1.2)
    (format' x)

let () = derived [1;2;3]
    
(*
let () =
  let ca = Custom_succ (Custom_zero) in
  Format.eprintf (Format ( Custom ( ca, (fun () -> fun x -> x),
                                    (Formatting_lit (Flush_newline, End_of_format))),
  "%A@.")) "gabababa"
*)
  

(* all we need now is a some filter which converts

  fprintf std_formatter "%A %A %A@." 1 1.2 [1;2;3]

to

  fprintf std_formatter "%t %t %t@." (format' 1) (format' 1.2) (format' [1;2;3])
*)

