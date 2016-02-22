(* Size of type *)

open Ppxx.Utils

open Types
open Btype

type t = (int option, int) Hashtbl.t
(** Polynomial. v_1 * 2 + v_2 * 3 + 4 has the following bindings:
    [(Some 1, 2); (Some 3, 2); (None, 4)]
*)

let to_string t =
  let open Printf in
  String.concat " + "
  & Hashtbl.fold (fun k v st ->
    let s = match k,v with
      | None, _ -> sprintf "%d" v
      | Some _, 0 -> ""
      | Some k, 1 -> sprintf "a%d" k
      | Some k, _ -> sprintf "%d*a%d" v k
    in
    s :: st) t []
    
let size ty =
  let open Hashtbl in
  let tbl = create 9 in
  let incr k =
    try
      replace tbl k (find tbl k + 1)
    with
    | Not_found -> add tbl k 1
  in
  let it = 
    { type_iterators with
      it_do_type_expr = (fun it ty ->
        let ty = Ctype.repr ty in
        begin match ty.desc with
        | Tvar _ -> incr (Some ty.id)
        | _ -> incr None
        end;
        type_iterators.it_do_type_expr it ty)
    }
  in
  it.it_type_expr it ty;
  unmark_iterators.it_type_expr unmark_iterators ty;
  tbl

let lt t1 t2 =
  (* Comparison of polynomials.
     All the components in t1 must appear in t2 with GE multiplier.
     At least one component in t1 must appear in t2 with GT multiplier.
  *)
  let open Hashtbl in
  try
    fold (fun k1 v1 found_gt ->
      let v2 = find t2 k1 in
      if v1 < v2 then true
      else if v1 = v2 then found_gt
      else raise Exit
      ) t1 false
  with
  | Exit | Not_found -> false

let has_var t =
  try
    Hashtbl.iter (fun k v -> if k <> None && v > 0 then raise Exit) t;
    false
  with
  | Exit -> true
  
