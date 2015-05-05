(* size of type *)
open Ppxx
open Types
open Btype

type t = (int option, int) Hashtbl.t

let to_string t =
  String.concat " + "
  & Hashtbl.fold (fun k v st ->
    let k = match k with
      | None -> ""
      | Some n -> Printf.sprintf " a%d" n
    in
    Printf.sprintf "%d%s" v k :: st) t []
    
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
  let open Hashtbl in
  try
    iter (fun k v ->
      try
        if find t1 k >= v then raise Exit
      with Not_found -> ()
    ) t2;
    true
  with
  | Exit -> false

(* hello world
 
 *)
