open Utils
open List
open Parsetree
open Format
open Ppxx
open Longident

type t = 
  | Just of Longident.t
(*
  | In of Longident.t
  | In_rec of Longident.t
  | Or of t list
  | Opened of t
*)

let to_string = 
  let open Format in 
  function
  | Just lid -> ksprintf (fun x -> x) "%a" Pprintast.default#longident lid

let prefix = "Policy_"

(* convert an arbitrary string to Lexer.identchar's
   '_' is a special char. 
 *)
let mangle s = 
  let len = String.length s in
  let b = Buffer.create len in
  Buffer.add_string b prefix;
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
  let prefix_len = String.length prefix in
  assert (String.sub s 0 prefix_len = prefix); (* better error handling *)
  let s = String.sub s prefix_len (String.length s - prefix_len) in
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
      | _ -> assert false (* CR jfuruse: need error handling *)
    end
  in
  f 0;
  Buffer.contents b

let from_string s = 
  let lexbuf = Lexing.from_string s in
  try Parser.parse_expression Lexer.token lexbuf with
  | e -> 
      eprintf "policy parse error at: %S@." s;
      raise e

let from_expression e = 
  match e.pexp_desc with
  | Pexp_construct ({txt=lid}, None) -> Just lid
  | _ -> failwith "illegal policy expression"

let from_structure str =
  match str with
  | [] -> failwith "implicit policy is empty"
  | _::_::_ -> failwith "multiple implicit policies are not allowed"
  | [sitem] ->
      match sitem.pstr_desc with
      | Pstr_eval (e, _) ->
          from_expression e, e.pexp_loc
      | _ ->
          failwith "policy must be an OCaml expression"

let from_payload = function
  | PStr s -> from_structure s
  | _ -> assert false (* CR jfuruse: better error handling *)

(* typed world *)

open Types

let from_module_type env mty =
(*
eprintf "from_module_type: @[%a@]@." Printtyp.modtype mty;
*)
  let sg = 
    try
      match Env.scrape_alias env @@ Mtype.scrape env mty with
      | Mty_signature sg -> sg
      | _ -> assert false
    with
    | e -> 
        eprintf "scraping failed: %s" & Printexc.to_string e;
        raise e
  in
  match 
    flip filter_map sg & function
      | Sig_type (id, tydecl, _) when id.Ident.name = "__imp_policy__" ->
          Some tydecl
      | _ -> None
  with
  | [] -> None
  | [ { type_params = []
      ; type_kind = Type_variant [ { cd_id= id; cd_args = []; cd_res = None; cd_loc = _loc} ]
      ; type_manifest = None } ] ->
      Some (from_expression & from_string & unmangle id.Ident.name)
  | [_] -> assert false (* CR jfuruse: better error handling *)
  | _ -> assert false

let candidates loc env = function
  | Just lid ->
      match 
        (* CR jfuruse: what is load parameter? *)  
        try Some (Env.lookup_module ~load:true lid env) with _ -> None
      with
      | None ->
          errorf "%a: no module found: %a" Location.print_loc loc Longident.format lid
      | Some path ->
          match 
            try Some (Env.find_module path env) with _ -> None
          with
          | None -> 
              errorf "%a: no module desc found: %a" Location.print_loc loc Path.format path
          | Some mdecl -> 
              let rec get_candidates env lid mty =
                let sg = 
                  try
                    match Env.scrape_alias env @@ Mtype.scrape env mty with
                    | Mty_signature sg -> sg
                    | _ -> assert false
                  with
                  | e -> 
                      eprintf "scraping failed: %s" & Printexc.to_string e;
                      raise e
                in
                flip2 fold_right sg [] & fun sitem st -> match sitem with
                | Sig_value (id, _vdesc) ->
                   (* CR jfuruse: 
                      I don't undrestand yet why the above _vdesc is not appropriate
                      and we must get vdesc like below.
                   *)
                    let lid = Ldot (lid, Ident.name id) in
                    begin try
                      let path, vdesc = Env.lookup_value lid env in
                      (lid, path, vdesc) :: st
                    with
                    | Not_found ->
                        warn (fun () -> 
                          eprintf "%%imp instance %a is not accessible in the current scope therefore ignored." Longident.format lid);
                        st
                    end

                | Sig_module (id, moddecl, _) -> 
                    let lid = Ldot (lid, Ident.name id) in
                    get_candidates env lid moddecl.Types.md_type @ st

                | _ -> st
              in
              get_candidates env lid mdecl.md_type
