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

(** spec dsl *)
type t = 
  | Or of t2 list
  | Type (** [%imp].  No allowed in [%%imp_policy] *)

and t2 = 
  | Opened of t3
  | Direct of t3
  | Aggressive of t2
  | Related

and t3 =
  | In of Longident.t
  | Just of Longident.t

let rec is_static = function
  | Opened _ -> true
  | Direct _ -> true
  | Aggressive t2 -> is_static t2
  | Related -> false
    
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
    | Related -> "related"
    | Aggressive x -> Printf.sprintf "aggressive (%s)" (t2 x)
  and t3 = function
    | In lid -> ksprintf (fun x -> x) "%a" Longident.format lid
    | Just lid -> ksprintf (fun x -> x) "just %a" Longident.format lid
  in
  t 

let prefix = "Policy_"
let prefix_len = String.length prefix

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
  try
    if not & String.is_prefix prefix s then raise Exit
    else
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
          | _ -> raise Exit
        end
      in
      f 0;
      `Ok (Buffer.contents b)
  with
  | Failure s -> `Error (`Failed_unmangle s)

let from_string s = 
  let lexbuf = Lexing.from_string s in
  try `Ok (Parser.parse_expression Lexer.token lexbuf) with
  | _ -> `Error (`Parse s)

let from_expression e = 
  try
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
          | None -> failwith "just requires an argument"
          end
      | Pexp_construct ({txt=lid}, None) -> In lid
      | _ -> failwith "illegal policy expression"
    in
    `Ok (t e)
  with
  | Failure s -> `Error (`ParseExp (e, s))

let from_structure str =
  match str with
  | [] -> `Ok Type
  | _::_::_ -> 
      `Error (`String "multiple implicit policies are not allowed")
  | [sitem] ->
      match sitem.pstr_desc with
      | Pstr_eval (e, _) ->
          from_expression e
      | _ ->
          `Error (`String "policy must be an OCaml expression")

let from_ok loc = function
  | `Ok v -> v
  | `Error (`String s) -> errorf "%a: %s" Location.print_loc loc s
  | `Error (`Failed_unmangle s) -> 
      errorf "%a: Illegal policy encoding: %S" Location.print_loc loc s
  | `Error (`Parse s) ->
      errorf "%a: Policy parse failed: %S" Location.print_loc loc s
  | `Error (`ParseExp (_, s)) ->
      errorf "%a: Policy parse failed: %s" Location.print_loc loc s

let from_payload = function
  | PStr s -> from_structure s
  | _ -> `Error (`String "policy must be an OCaml expression")

(* typed world *)

open Types

let from_type_decl loc = function
  | { type_params = []
    ; type_kind = Type_variant [ { cd_id= id; cd_args = []; cd_res = None; cd_loc = loc} ]
    ; type_manifest = None } ->
      let (>>=) x f = match x with `Error e -> `Error e | `Ok v -> f v in
      from_ok loc & 
        unmangle id.Ident.name >>= fun x -> 
        from_string x >>= fun x ->
        from_expression x
  | _ -> 
      errorf "%a: Illegal data type definition for __imp_policy__. [%%%%imp_policy POLICY] must be used." Location.print_loc loc

let from_module_type env mp loc mty =
  let sg = 
    try
      match Env.scrape_alias env @@ Mtype.scrape env mty with
      | Mty_signature sg -> sg
      | _ -> assert false
    with
    | _ -> 
        errorf "%a: Scraping failure of module %a"
          Location.print_loc loc
          Path.format mp
  in
  match 
    flip filter_map sg & function
      | Sig_type (id, tydecl, _) when id.Ident.name = "__imp_policy__" ->
          Some tydecl
      | _ -> None
  with
  | [] -> None
  | [td] -> Some (from_type_decl loc td)
  | _ -> assert false

let from_module_path env mp =
  let md = Env.find_module mp env in (* CR jfuruse: Error *)
  match from_module_type env mp md.md_loc md.md_type with
  | None -> 
      errorf "%a: Module %a has no implicit policy declaration [%%%%imp_policy POLICY]@." 
        Location.print_loc md.md_loc
        Path.format mp
  | Some policy -> policy

let check_module env loc lid =
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

(** result *)
type res =
  | Static of Ppxx.Longident.t * Path.t * Types.value_description
  | Dynamic of (Types.type_expr -> (Ppxx.Longident.t * Path.t * Types.value_description) list)
          
let check_value env lid =
  try
    let path, vdesc = Env.lookup_value lid env in
    Some (path, vdesc)
  with
  | Not_found ->
      warn (fun () -> 
        eprintf "%%imp instance %a is not accessible in the current scope therefore ignored." Longident.format lid);
      None
  
let check_module_path_accessibility env loc path =
  let lid = Untypeast.lident_of_path path in
  try
    if path <> Env.lookup_module ~load:true (* CR jfuruse: ? *) lid env then begin
      warn (fun () ->
        eprintf "%a: %a is not accessible in the current scope therefore ignored." Location.print_loc loc Path.format path);
      `Shadowed
    end else
      `Accessible (lid, Env.find_module path env)
  with
  | _ ->
      warn (fun () ->
        eprintf "%a: ?!?!? %a is not found in the environment." Location.print_loc loc Path.format path);
      `Not_found
  
    
let scrape_sg env mdecl = 
  try
    match Env.scrape_alias env & Mtype.scrape env mdecl.md_type with
    | Mty_signature sg -> sg
    | Mty_functor _ -> [] (* We do not scan the internals of functors *)
    | _ -> assert false
  with
  | e -> 
      eprintf "scraping failed: %s" & Printexc.to_string e;
      raise e

let rec values_of_module ~recursive env lid mdecl =
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
      | Some (path, vdesc) -> (lid, path, vdesc, false) :: st
      end

  | Sig_module (id, moddecl, _) when recursive -> 
      let lid = Ldot (lid, Ident.name id) in
      values_of_module ~recursive env lid moddecl @ st
        
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
          Some (Env.lookup_module ~load:true (*?*) lid env)
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
          Some (Env.lookup_module ~load:true (*?*) lid env)
        with
        | _ -> None)
      
let module_lids_in_open_paths env lids opens =
  concat_map (module_lids_in_open_path env lids) opens

let data_types env ty =
  let open Btype in
  let open Ctype in
  let res = ref [] in
  (* CR jfuruse: oops, may loop forever? *)
  let expand_repr_desc env ty = (repr & expand_head env ty).desc in
  let rec loop ty = 
    begin match expand_repr_desc env ty with
    | Tconstr (p, _tys, _) ->
        res := p :: !res;
    | _ -> ()
    end;
    iter_type_expr loop ty
  in
  loop ty;
  sort_uniq compare !res

let related_modules env ty =
  let open Path in
  data_types env ty
  |> filter_map (function
      | Pdot (p, _, _) -> Some p
      | Pident _ -> None
      | Papply _ -> assert false)
  |> sort_uniq compare

let cand_direct env loc t3 =
  let recursive, lid = match t3 with
    | Just lid -> false, lid
    | In lid -> true, lid
  in
  let mdecl = check_module env loc lid in
  values_of_module ~recursive env lid mdecl

let cand_related env loc ty = 
  let mods = related_modules env ty in
  let lmods = filter_map (fun p ->
    match check_module_path_accessibility env loc p with
    | `Accessible (lid, mdecl) -> Some (lid, mdecl)
    | _ -> None) mods
  in
  (* CR jfuruse: values_of_module should be memoized *)
  concat & map (fun (lid,mdecl) -> values_of_module ~recursive:false env lid mdecl) lmods
  
let cand_opened env loc x =
  let lid = match x with
    | Just lid -> lid
    | In lid -> lid
  in
  let opens = get_opens env in
  let paths = 
    concat 
    & map (module_lids_in_open_path env [lid]) 
    & None :: map (fun x -> Some x) opens
  in
  let lids = flip filter_map paths (fun path ->
    match check_module_path_accessibility env loc path with
    | `Accessible (lid,_) -> Some lid
    | `Shadowed | `Not_found -> None)
  in
  concat & map (fun lid ->
    cand_direct env loc
      (match x with
      | Just _ -> Just lid
      | In _ -> In lid)) lids

let rec cand_static env loc = function
  | Related -> assert false
  | Aggressive x ->
      map (fun (l,p,v,_f) -> (l,p,v,true)) & cand_static env loc x
  | Opened x -> cand_opened env loc x
  | Direct x -> cand_direct env loc x

let rec cand_dynamic env loc ty = function
  | Related -> cand_related env loc ty
  | Aggressive x ->
      map (fun (l,p,v,_f) -> (l,p,v,true)) & cand_dynamic env loc ty x
  | Opened _ | Direct _ -> assert false

let uniq xs =
  let tbl = Hashtbl.create 107 in
  iter (fun (l,p,v,f as x) ->
    try
      let (_, _, _, f') = Hashtbl.find tbl p in
      Hashtbl.replace tbl p (l,p,v, f || f')
    with
    | Not_found -> Hashtbl.add tbl p x) xs;
  Hashtbl.to_list tbl |> map snd

let candidates env loc = function
  | Type -> assert false (* This should not happen *)
  | Or ts ->
      let statics, dynamics = partition is_static ts in
      let statics = concat & map (cand_static env loc) statics in
      let dynamics ty = concat & map (cand_dynamic env loc ty) dynamics in
      fun ty -> uniq & statics @ dynamics ty


