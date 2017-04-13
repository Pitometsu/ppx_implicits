open Ppxx.Utils
open List

open Ppxx.Compilerlib
open Typpx.Compilerlib
open Longident
open Path
open Types

module String = struct
  include Ppxx.Utils.String

  let drop len str = String.sub str len (String.length str - len)

  let is_prefix ?(from=0) sub str =
    let sublen = String.length sub in
    try 
      if String.sub str from sublen = sub then Some (drop (from + sublen) str)
      else None
    with _ -> None
end 

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
  let lid = (* Typpx.*) Untypeast.lident_of_path path in      
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
  let () = !!% "dummy_module of @[%a@]@." Printtyp.modtype mty in 
*)
  let dummy = "Dummy" in
  let id = Ident.create "Dummy" in
  let env = Env.add_module id mty Env.empty in
object

  method lookup_type s =
    match Env.lookup_type (Longident.(Ldot (Lident dummy, s))) env with
    | Pdot (_, s', n) as p ->
       let td = Env.find_type p env in
       Pdot (mp, s', n), td
    | _ -> assert false (* impos *)

  method lookup_module s =
    match Env.lookup_module ~load:false (Longident.(Ldot (Lident dummy, s))) env with
    | Pdot (_, s', n) -> Pdot (mp, s', n)
    | _ -> assert false (* impos *)
    
  method lookup_value s =
    match Env.lookup_value (Longident.(Ldot (Lident dummy, s))) env with
    | Pdot (_, s', n), _vd -> Pdot (mp, s', n)
    | _ -> assert false (* impos *)
    
end

let exit_then d f = try f () with Exit -> d

module Result = struct
  type ('a, 'err) t =
    [ `Ok of 'a
    | `Error of 'err
    ]

  let from_Ok f = function
    | `Ok v -> v
    | `Error e -> f e

  module Monad = struct
    let (>>=) x f = match x with `Error e -> `Error e | `Ok v -> f v
  end

end

let from_Ok = Result.from_Ok

module Option = struct
  include Ppxx.Utils.Option
  exception Is_None
  let from_Some = function
    | Some x -> x
    | None -> raise Is_None
  module Monad = struct
    let return x = Some x
    let some = return
    let (>>=) m f = match m with
      | None -> None
      | Some x -> f x
  end
end

let from_Some = Option.from_Some
  
module List = struct
  include Ppxx.Utils.List
    
  (* Haskell's splitAt. Borrowed from Spotlib. Tested. *)
  let split_at n xs =
    let rec split_at_ n st xs =
      if n <= 0 then st, xs
      else match xs with
      | [] -> st, []
      | x::xs -> split_at_ (n-1) (x::st) xs
    in
    let r, dropped = split_at_ n [] xs in
    rev r, dropped
end

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
  | Failure e -> `Error (`Failed_unmangle (s ^ ": " ^ e))

let expression_from_string s = 
  let lexbuf = Lexing.from_string s in
  try `Ok (Parser.parse_expression Lexer.token lexbuf) with
  | _ -> `Error (`Parse s)

let tvars_of_core_type cty =
  (* Using mapper for iterator is redundant, but easiest way *)
  let open Ast_mapper in
  let open Parsetree in
  let vars = ref [] in
  let extend super =
    let typ self ty =
      match ty.ptyp_desc with
      | Ptyp_var s ->
          if not & mem s !vars then vars := s :: !vars;
          ty
      | _ -> super.typ self ty
    in
    { super with typ }
  in
  let m = extend default_mapper in
  ignore & m.typ m cty;
  !vars

let sig_module_of_stri sitem =
  let open Parsetree in
  match sitem.pstr_desc with
  | Pstr_modtype mtd ->
      Ppxx.Helper.Sig.module_ ~loc:sitem.pstr_loc
        { pmd_name       = mtd.pmtd_name
        ; pmd_type       = from_Some mtd.pmtd_type
        ; pmd_attributes = mtd.pmtd_attributes
        ; pmd_loc        = mtd.pmtd_loc
        }
  | _ ->
      !!% "sig_module_of_stri: got a non modtype@.";
      assert false

let rec values_of_module ~recursive env path mdecl : Path.t list =
  let m = new dummy_module env path mdecl.md_type in
  let sg = scrape_sg path env mdecl in
  flip2 fold_right sg [] & fun sitem st -> match sitem with
    | Sig_value (id, _vdesc) ->
        let path = try m#lookup_value & Ident.name id with Not_found ->
          !!% "values_of_module: m#lookup_value %s not found@." & Ident.name id;
          assert false
        in
        path :: st
    | Sig_module (id, moddecl, _) when recursive -> 
        let path = m#lookup_module & Ident.name id in
        values_of_module ~recursive env path moddecl @ st
          
    | _ -> st

let check_module env loc path =
  match 
    try Some (Env.find_module path env) with _ -> None
  with
  | None -> 
      raise_errorf "%a: no module desc found: %a" Location.format loc Path.format path
  | Some mdecl -> mdecl

let values_of_module ~recursive env loc path =
  let mdecl = check_module env loc path in
  values_of_module ~recursive env path mdecl

let format_expression ppf e =
  Pprintast.expression ppf
  & Unembed.unembed
  & (* Typpx. *) Untypeast.(default_mapper.expr default_mapper) e

let is_none e = match e.Typedtree.exp_desc with
  | Texp_construct ({Location.txt=Lident "None"}, _, []) -> 
      begin match is_option_type e.exp_env e.exp_type with
      | None ->
          !!% "is_none: the input is type-corrupted@."; assert false
      | Some ty -> Some ty
      end
  | _ -> None
