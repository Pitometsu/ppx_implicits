(*

  Instance search space specification DSL, magling to and back from
  OCaml type definitions.

*)
open Utils
open List
open Parsetree
open Format
open Compilerlibx
open Longident
open Path

(** spec dsl *)
type t = 
  | Or of t2 list
  | Type (** [%imp].  Encoded as a type definition.  No allowed in [%%imp_policy] *)

and t2 = 
  | Opened of Longident.t flagged
  | Direct of (Longident.t * Path.t option) flagged
  | Aggressive of t2
  | Related
  | Name of string * Re.re * t2
  | Opened2 of Path.t option

and 'a flagged = In of 'a | Just of 'a

let rec is_static = function
  | Opened _ -> true
  | Direct _ -> true
  | Related -> false
  | Aggressive t2 | Name (_, _, t2) -> is_static t2
  | Opened2 _ -> true
    
let to_string = 
  let flagged f = function
    | In x -> f x
    | Just x -> Printf.sprintf "just %s" & f x
  in
  let rec t = function
    | Type -> ""
    | Or [] -> assert false
    | Or [x] -> t2 x
    | Or xs -> String.concat ", " (map t2 xs)
  and t2 = function
    | Direct pf -> flagged (fun (l,_) -> Longident.to_string l) pf
    | Opened lf -> Printf.sprintf "opened (%s)" (flagged Longident.to_string lf)
    | Related -> "related"
    | Aggressive x -> Printf.sprintf "aggressive (%s)" (t2 x)
    | Name (s, _re, x) -> Printf.sprintf "name %S (%s)" s (t2 x)
    | Opened2 _ -> "opened2"
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

let to_mangled_string x = mangle & to_string x

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
      | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "aggressive"} },
                    ["", e] ) -> Aggressive (t2 e)
      | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "opened"} },
                    ["", e] ) -> 
          Opened (flag_lid e)
      | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "name"} },
                    [ "", { pexp_desc = Pexp_constant (Const_string (s, _)) }
                    ; "", e ] ) -> Name (s, Re_pcre.regexp s, t2 e)
      | Pexp_ident {txt=Lident "related"} -> Related
      | Pexp_ident {txt=Lident "opened2"} -> Opened2 None
      | _ -> 
          let flid = flag_lid e in
          begin match flid with
          | In lid -> Direct (In (lid, None))
          | Just lid -> Direct (Just (lid, None))
          end
    and flag_lid e = match e.pexp_desc with
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
  | `Error (`String s) -> errorf "%a: %s" Location.format loc s
  | `Error (`Failed_unmangle s) -> 
      errorf "%a: Illegal policy encoding: %S" Location.format loc s
  | `Error (`Parse s) ->
      errorf "%a: Policy parse failed: %S" Location.format loc s
  | `Error (`ParseExp (_, s)) ->
      errorf "%a: Policy parse failed: %s" Location.format loc s

let from_payload = function
  | PStr s -> from_structure s
  | _ -> `Error (`String "policy must be an OCaml expression")

(* typed world *)

open Types

let fix_opened2 _loc p = function
  | Type -> assert false
  | Or t2s ->
      let f = function
        | Opened2 None -> Opened2 (Some p)
        | Opened2 (Some _) -> assert false
        | x -> x
      in
      Or (map f t2s)
    
let from_type_decl p loc = function
  | { type_params = []
    ; type_kind = Type_variant [ { cd_id= id; cd_args = []; cd_res = None; cd_loc = loc} ]
    ; type_manifest = None } ->
      let (>>=) x f = match x with `Error e -> `Error e | `Ok v -> f v in
      fix_opened2 loc p & from_ok loc
      & unmangle id.Ident.name >>= from_string >>= from_expression
  | _ -> 
      errorf "%a: Illegal data type definition for __imp_policy__. [%%%%imp_policy POLICY] must be used." Location.format loc

let from_module_type mp loc mty =
  let dummy = "Dummy" in
  let id = Ident.create dummy in
  let env = Env.add_module id mty Env.empty in
  (* note that td is under Dummy *)
  let p, td =
    try
      let p,type_decl = Env.lookup_type (Longident.(Ldot (Lident dummy, "__imp_policy__"))) env
      in
      match p with
      | Pdot (_, "__imp_policy__", n) ->
          Pdot (mp, "__imp_policy__", n), type_decl
      | _ -> assert false
    with
    | Not_found ->
        assert false (* CR jfuruse: error *)
  in
  (* Add mp.Instances as the default recipes *)
  match from_type_decl p loc td with
  | Type -> assert false
  | Or t2s ->
      let default_instances =
        try
          let p = Env.lookup_module ~load:false (*?*) (Longident.(Ldot (Lident dummy, "Instances"))) env in
          match p with
          | Pdot (_, "Instances", n) ->
              let p = Pdot (mp, "Instances", n) in
              [ Direct (In (Untypeast.lident_of_path p, Some p)) ]
          | _ -> assert false
        with
        | Not_found -> []
      in
      Some (Or (t2s @ default_instances))

let from_module_path env mp =
  let md = Env.find_module mp env in (* CR jfuruse: Error *)
  match from_module_type mp md.md_loc md.md_type with
  | None -> 
      errorf "%a: Module %a has no implicit policy declaration [%%%%imp_policy POLICY]@." 
        Location.format md.md_loc
        Path.format mp
  | Some res -> res

let check_module env loc path =
  match 
    try Some (Env.find_module path env) with _ -> None
  with
  | None -> 
      errorf "%a: no module desc found: %a" Location.format loc Path.format path
  | Some mdecl -> mdecl

let check_module_path_accessibility env loc path =
  let lid = Untypeast.lident_of_path path in
  try
    if path <> Env.lookup_module ~load:true (* CR jfuruse: ? *) lid env then begin
      warn (fun () ->
        eprintf "%a: %a is not accessible in the current scope therefore ignored." Location.format loc Path.format path);
      `Shadowed
    end else
      `Accessible (lid, Env.find_module path env)
  with
  | _ ->
      warn (fun () ->
        eprintf "%a: ?!?!? %a is not found in the environment." Location.format loc Path.format path);
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

let rec values_of_module ~recursive env lid path mdecl =
  let sg = scrape_sg env mdecl in
  flip2 fold_right sg [] & fun sitem st -> match sitem with
  | Sig_value (id, _vdesc) ->
      (* CR jfuruse: 
         I don't undrestand yet why the above _vdesc is not appropriate
         and we must get vdesc like below.

         I guess some type alias information is not in _vdesc...
      *)
      let lid = Ldot (lid, Ident.name id) in
      let path = Path.Pdot (path, Ident.name id, id.Ident.stamp) in (* CR jfuruse: likely incorrect *)
      begin 
        (* CR jfuruse: think about error *)
        let vdesc = Env.find_value path env in
        (lid, path, vdesc, false) :: st
      end
  | Sig_module (id, moddecl, _) when recursive -> 
      let lid = Ldot (lid, Ident.name id) in
      let path = Path.Pdot (path, Ident.name id, id.Ident.stamp) in (* CR jfuruse: likely incorrect *)
      (* CR jfuruse: Can I trust moddecl? *)
      values_of_module ~recursive env lid path moddecl @ st
        
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

let rec dump_summary =
  let open Format in
  function
  | Env.Env_empty -> ()
  | Env_value (s, _, _)
  | Env_extension (s, _, _)
  | Env_modtype (s, _, _)
  | Env_class (s, _, _)
  | Env_cltype (s, _, _)
  | Env_functor_arg (s, _) -> dump_summary s
  | Env_type (s, id, _) -> eprintf "type %a@." Ident.format id; dump_summary s
  | Env_module (s, id, _) -> eprintf "module %a@." Ident.format id; dump_summary s
  | Env_open (s, path) -> eprintf "open %a@." Path.format path; dump_summary s

let dump_summary env = dump_summary & Env.summary env
  
let module_lids_in_open_path env lids = function
  | None -> 
      (* Finds lids in the current scope, but only defined ones in the current scope level.
           * Persistent ones are excluded
           * Sub-modules are excluded
      *)
      flip filter_map lids (fun lid ->
        try
          let p = Env.lookup_module ~load:true (*?*) lid env in
          match p with
          | Pident id when not & Ident.persistent id -> Some p
          | _ -> None (* not sure... *)
        with
        | _ -> None)
  | Some open_ ->
      (*  eprintf "open %a@." Path.format open_; *)
      let mdecl = Env.find_module open_ env in (* It should succeed *)
      let sg = scrape_sg env mdecl in
      let env = Env.open_signature Asttypes.Fresh open_ sg Env.empty in
      flip filter_map lids (fun lid ->
        try
          Some (Env.lookup_module ~load:true (*?*) lid env)
        with
        | _ -> None)
      
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
  let recursive, lid, popt = match t3 with
    | Just (lid, popt) -> false, lid, popt
    | In (lid, popt) -> true, lid, popt
  in
  let path = match popt with
    | Some p -> p
    | None -> 
        (* CR jfuruse: error handling *)
        Env.lookup_module ~load:true lid env
  in
  let mdecl = check_module env loc path in
  values_of_module ~recursive env lid path mdecl

let cand_related env _loc ty = 
  let mods = related_modules env ty in
  let lmods = flip map mods & fun p ->
    match Unshadow.check_module_path env p with
    | `Accessible lid ->
        let mdecl = Env.find_module p env in
        (lid, p, mdecl)             
    | `Shadowed (id, id', p) ->
        Unshadow.aliases := (id, id') :: !Unshadow.aliases;
        let mdecl = Env.find_module p env in
        (Untypeast.lident_of_path p, p, mdecl)             
    | `Not_found _p -> assert false (* CR jfuruse: need error handling *)
  in
  (* CR jfuruse: values_of_module should be memoized *)
  concat & map (fun (lid,path,mdecl) -> values_of_module ~recursive:false env lid path mdecl) lmods
  
let cand_opened env loc x =
  dump_summary env;
  let lid = match x with
    | Just lid -> lid
    | In lid -> lid
  in
  let opens = get_opens env in
  if !Ppxx.debug_resolve then begin
    Format.eprintf "debug_resolve: cand_opened opened paths@.";
    flip iter opens & Format.eprintf "  %a@." Path.format
  end;
  let paths = 
    concat 
    & map (module_lids_in_open_path env [lid]) 
    & None :: map (fun x -> Some x) opens
  in
  if !Ppxx.debug_resolve then begin
    Format.eprintf "debug_resolve: cand_opened cand modules@.";
    flip iter paths & Format.eprintf "  %a@." Path.format
  end;
  let lids = flip map paths & fun path ->
    match Unshadow.check_module_path env path with
    | `Accessible lid -> lid
    | `Shadowed (id, id', p) ->
        Unshadow.aliases := (id, id') :: !Unshadow.aliases;
        Untypeast.lident_of_path p
    | `Not_found _p -> assert false (* CR jfuruse: need error handling *)
  in
  concat & map2 (fun lid path ->
    cand_direct env loc
      (match x with
      | Just _ -> Just (lid, Some path)
      | In _ -> In (lid, Some path))) lids paths

(* [%imp] : 'a M.ty
   type M.__imp_policy__ must exists and it is "opened2"
   We seek types equal to __imp_instance__ = M.__imp_policy__
*) 
let cand_opened2 env loc p_policy =
  let has_instance mp =
    let md = Env.find_module mp env in
    let dummy = "Dummy" in
    let id = Ident.create dummy in
    let env = Env.add_module id md.md_type Env.empty in
    (* note that td is under Dummy *)
    try
      let p, td = Env.lookup_type (Longident.(Ldot (Lident dummy, "__imp_instance__"))) env
      in
      begin match p with
      | Pdot (_, "__imp_instance__", n) ->
          begin match td with
          | { type_params = [];
              type_manifest = Some { desc = Tconstr (p, _, _) };
            } when p = p_policy ->
              Some (Pdot (mp, "__imp_instance__", n))
          | _ -> None
          end
      | _ -> assert false
      end
    with
    | Not_found -> None
  in
  let rec find_modules = function
    | Env.Env_empty -> []
    | Env_value (s, _, _)
    | Env_extension (s, _, _)
    | Env_modtype (s, _, _)
    | Env_class (s, _, _)
    | Env_cltype (s, _, _)
    | Env_functor_arg (s, _)
    | Env_type (s, _, _) -> find_modules s
    | Env_module (s, id, md) ->
        let res = find_modules s in
        begin match has_instance (Pident id) with
        | None -> res
        | Some x -> x :: res
        end
    | Env_open (s, path) ->
        let md = Env.find_module path env in
        fold_left (fun res -> function
          | Sig_module (id, md, _) ->
              begin match has_instance (Pdot (path, id.Ident.name, id.Ident.stamp (* right? *))) with
              | None -> res
              | Some x -> x :: res
              end
          | _ -> res)
          (find_modules s) (scrape_sg env md)
  in
  let ps = find_modules & Env.summary env in
  assert false
  
let cand_name rex f =
  filter (fun (lid, _, _, _) ->
    Re_pcre.pmatch ~rex & Longident.to_string lid) & f ()

let rec cand_static env loc = function
  | Related -> assert false
  | Aggressive x ->
      map (fun (l,p,v,_f) -> (l,p,v,true)) & cand_static env loc x
  | Opened x -> cand_opened env loc x
  | Direct x -> cand_direct env loc x
  | Name (_, rex, t2) -> cand_name rex & fun () -> cand_static env loc t2
  | Opened2 p -> cand_opened2 env loc p

let rec cand_dynamic env loc ty = function
  | Related -> cand_related env loc ty
  | Aggressive x ->
      map (fun (l,p,v,_f) -> (l,p,v,true)) & cand_dynamic env loc ty x
  | Opened _ | Direct _ | Opened2 _ -> assert false
  | Name (_, rex, t2) -> cand_name rex & fun () -> cand_dynamic env loc ty t2

type result = Longident.t * Path.t * value_description * bool (* bool : aggressive *)

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
      if !Ppxx.debug_resolve then begin
        Format.eprintf "debug_resolve: static candidates@.";
        flip iter statics & fun (lid, path, _vdesc, _b) ->
          Format.eprintf "  %a %a@." Longident.format lid Path.format path
      end;
      let dynamics ty = concat & map (cand_dynamic env loc ty) dynamics in
      fun ty -> uniq & statics @ dynamics ty
