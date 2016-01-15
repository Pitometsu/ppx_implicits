open Ppxx.Utils
open List

open Ppxx.Compilerlib
open Longident
open Path
open Types

(* CR jfuruse: _path is not used *)      
let scrape_sg _path env mdecl = 
  try
    match Env.scrape_alias env & Mtype.scrape env mdecl.md_type with
    | Mty_signature sg ->
(*
        test_scrape_sg path env sg;
*)
        sg
    | Mty_functor _ -> [] (* We do not scan the internals of functors *)
    | _ -> assert false
  with
  | e -> 
      !!% "scraping failed: %s" & Printexc.to_string e;
      raise e

let _test_scrape_sg path env sg =
  match path with
  | Pident {Ident.name = "Pervasives"} -> ()
  | _ ->
  let lid = Typpx.Untypeast.lident_of_path path in      
  !!% "SCRAPE SG %a@." Path.format_verbose path;
  flip iter sg & function
    | Sig_value (id, _vdesc) ->
        let lid = Ldot (lid, id.Ident.name) in
        let popt = try Some (fst (Env.lookup_value lid env)) with _ -> None in
        !!% "  value %a  >> %a@." Ident.format_verbose id (Option.format Path.format_verbose) popt
    | Sig_module (id, _moddecl, _) ->
        !!% "  module %a@." Ident.format_verbose id
    | Sig_type (id, _, _) -> 
        !!% "  type %a@." Ident.format_verbose id
    | _ -> ()
    
(** Build an empty type env except mp module with the given module type *)
class dummy_module env mp mty =
  (* Env.lookup_* does not support Mty_alias (and probably Mty_indent) *)
  let mty = Env.scrape_alias env & Mtype.scrape env mty in
(*
  let () = eprintf "dummy_module of @[%a@]@." Printtyp.modtype mty in 
*)
  let dummy = "Dummy" in
  let id = Ident.create "Dummy" in
  let env = Env.add_module id mty Env.empty in
object

  method lookup_type s =
    match Env.lookup_type (Longident.(Ldot (Lident dummy, s))) env with
    | Pdot (_, s', n), td -> Pdot (mp, s', n), td
    | _ -> assert false

  method lookup_module s =
    match Env.lookup_module ~load:false (Longident.(Ldot (Lident dummy, s))) env with
    | Pdot (_, s', n) -> Pdot (mp, s', n)
    | _ -> assert false
    
  method lookup_value s =
    match Env.lookup_value (Longident.(Ldot (Lident dummy, s))) env with
    | Pdot (_, s', n), _vd -> Pdot (mp, s', n)
    | _ -> assert false
    
end

let exit_then d f = try f () with Exit -> d

let mangle s = 
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    let c = String.unsafe_get s i in
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '\'' -> Buffer.add_char b c
    | '_' -> Buffer.add_string b "__"
    | _ -> 
        Buffer.add_char b '_';
        Buffer.add_string b & Printf.sprintf "%02x" & Char.code c
  done;
  Buffer.contents b

(* CR jfuruse: need tests *)
let unmangle s = 
  try
    let len = String.length s in
    let b = Buffer.create len in
    let rec f i = 
      if i = len then ()
      else begin
        let c = String.unsafe_get s i in
        match c with
        | 'A'..'Z' | 'a'..'z' | '0'..'9' | '\'' -> Buffer.add_char b c; f & i+1
        | '_' -> 
            begin match s.[i+1] with
            | '_' -> Buffer.add_char b '_'; f & i+2
            | _ ->
                let hex = String.sub s (i+1) 2 in
                let c = Char.chr & int_of_string & "0x" ^ hex in
                Buffer.add_char b c;
                f & i+3
            end
        | _ -> raise Exit
      end
    in
    f 0;
    `Ok (Buffer.contents b)
  with
  | Failure s -> `Error (`Failed_unmangle s)

module Result = struct
  let (>>=) x f = match x with `Error e -> `Error e | `Ok v -> f v
  let ok x = `Ok x
  let return = ok
  let error x = `Error x
  let protect f = try `Ok (f ()) with e -> `Error (`Exn e)
  let map_error f = function
    | `Ok v -> `Ok v
    | `Error e -> `Error (f e)
  let rec mapM f = function
    | [] -> ok []
    | x::xs ->
        f x >>= fun y -> mapM f xs >>= fun ys -> return (y :: ys)
end
  
