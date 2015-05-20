(*

  Instance search space specification DSL, magling to and back from
  OCaml type definitions.

*)
open Utils
open List
open Parsetree
open Format
open Ppxx
open Longident

type t = 
  | Or of t2 list
  | Type

and t2 = 
  | Opened of t3
  | Direct of t3

and t3 =
  | In of Longident.t
  | Just of Longident.t

let to_string = 
  let open Format in
  let rec t = function
    | Type -> ""
    | Or [] -> assert false
    | Or [x] -> t2 x
    | Or xs -> String.concat ", " (map t2 xs)
  and t2 = function
    | Direct x -> t3 x
    | Opened x -> Printf.sprintf "opened (%s)" (t3 x)
  and t3 = function
    | In lid -> ksprintf (fun x -> x) "%a" Pprintast.default#longident lid
    | Just lid -> ksprintf (fun x -> x) "just %a" Pprintast.default#longident lid
  in
  t 

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
  let get_lid e = match e.pexp_desc with
    | Pexp_construct ({txt=lid}, None) -> Some lid
    | _ -> None
  in
  let rec t e = match e.pexp_desc with
    | Pexp_tuple xs -> Or (map t2 xs)
    | _ -> Or [t2 e]
  and t2 e = match e.pexp_desc with
    | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "opened"} },
                  ["", e] ) -> Opened (t3 e)
    | _ -> Direct (t3 e)
  and t3 e = match e.pexp_desc with
    | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "just"} },
                  ["", e] ) -> 
        begin match get_lid e with
        | Some lid -> Just lid
        | None -> assert false (* error *)
        end
    | Pexp_construct ({txt=lid}, None) -> In lid
    | _ -> failwith "illegal policy expression"
  in
  t e

let from_structure str =
  match str with
  | [] -> Type, Location.none
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

let from_module_path env mp =
  let md = Env.find_module mp env in (* CR jfuruse: Error *)
  match from_module_type env md.md_type with
  | None -> 
      eprintf "policy not found in %a@." Printtyp.modtype md.md_type;
      assert false (* error *)
  | Some policy -> policy

let check_module loc env lid =
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
      | Some mdecl -> mdecl

let check_value env lid =
  try
    let path, vdesc = Env.lookup_value lid env in
    Some (path, vdesc)
  with
  | Not_found ->
      warn (fun () -> 
        eprintf "%%imp instance %a is not accessible in the current scope therefore ignored." Longident.format lid);
      None
  
let scrape_sg env mdecl = 
  try
    match Env.scrape_alias env & Mtype.scrape env mdecl.md_type with
    | Mty_signature sg -> sg
    | _ -> assert false
  with
  | e -> 
      eprintf "scraping failed: %s" & Printexc.to_string e;
      raise e

let rec get_candidates ~recursive env lid mdecl =
  let sg = scrape_sg env mdecl in
  flip2 fold_right sg [] & fun sitem st -> match sitem with
  | Sig_value (id, _vdesc) ->
      (* CR jfuruse: 
         I don't undrestand yet why the above _vdesc is not appropriate
         and we must get vdesc like below.
      *)
      let lid = Ldot (lid, Ident.name id) in
      begin match check_value env lid with
      | None -> st
      | Some (path, vdesc) -> (lid, path, vdesc) :: st
      end

  | Sig_module (id, moddecl, _) when recursive -> 
      let lid = Ldot (lid, Ident.name id) in
      get_candidates ~recursive env lid moddecl @ st
        
  | _ -> st

let rec get_opens = function
  | Env.Env_empty -> []
  | Env_value (s, _, _)
  | Env_type (s, _, _)
  | Env_extension (s, _, _)
  | Env_module (s, _, _)
  | Env_modtype (s, _, _)
  | Env_class (s, _, _)
  | Env_cltype (s, _, _)
  | Env_functor_arg (s, _) -> get_opens s
  | Env_open (s, path) -> path :: get_opens s

let get_opens env = get_opens & Env.summary env

let module_lids_in_open_path env lids = function
  | None -> 
      flip filter_map lids (fun lid ->
        try
          Some (Env.lookup_module ~load:false (*?*) lid env)
        with
        | _ -> None)
  | Some (Path.Pident id) when Ident.name id = "Pervasives" && Ident.persistent id  -> 
      (* We assume Pervasives has no instances *)
      []
  | Some open_ ->
      (*  eprintf "open %a@." Path.format open_; *)
      let mdecl = Env.find_module open_ env in (* It should succeed *)
      let sg = scrape_sg env mdecl in
      let env = Env.open_signature Asttypes.Fresh open_ sg env in
      flip filter_map lids (fun lid ->
        try
          Some (Env.lookup_module ~load:false (*?*) lid env)
        with
        | _ -> None)
      
let module_lids_in_open_paths env lids opens =
  concat_map (module_lids_in_open_path env lids) opens

let candidates loc env = 
  let rec t = function
    | Type -> assert false (* This should not happen *)
    | Or ts -> concat & map t2 ts
  and t2 = function
    | Opened x -> 
        let lid = match x with
          | Just lid -> lid
          | In lid -> lid
        in
        let make lid = match x with
          | Just _ -> Direct (Just lid)
          | In _ -> Direct (In lid)
        in
        let opens = get_opens env in
        let paths = 
          concat 
          & map (module_lids_in_open_path env [lid]) 
          & None :: map (fun x -> Some x) opens
        in
        (* We translate paths to longidents.
           for the instance name spacing by 'open', it seems ok.

           module X = struct
             module Show = struct
             end
           end
           module Y = struct
             module Show = struct
             end
           end
           open X
           open Y

           To access values inside X.Show, Show is not good since
           it is shadowed by open Y. 
        *)
        (* CR jfuruse: need to check lids are really point paths *)
        let lids = map Untypeast.lident_of_path paths in
        t & Or (map make lids)
    | Direct x -> t3 x
  and t3 = function
    | Just lid ->
        let mdecl = check_module loc env lid in
        get_candidates ~recursive:false env lid mdecl
    | In lid ->
        let mdecl = check_module loc env lid in
        get_candidates ~recursive:true env lid mdecl
  in
  t

let candidates loc env t =
  sort_uniq (fun (l1,p1,_) (l2,p2,_) -> compare (l1,p1) (l2,p2)) 
  & candidates loc env t
