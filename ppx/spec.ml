(*

  Instance search space specification DSL, magling to and back from
  OCaml type definitions.

*)
open Utils
open List
open Parsetree
open Format
open Ppxx.Compilerlib
open Longident
open Path

module Forge = Typpx.Forge
  
module Candidate = struct
  type t = {
    lid        : Longident.t;
    path       : Path.t;
    expr       : Typedtree.expression;
    type_      : Types.type_expr;
    aggressive : bool
  }

  let uniq xs =
    let tbl = Hashtbl.create 107 in
    iter (fun x ->
      try
        let x' = Hashtbl.find tbl x.path in
        Hashtbl.replace tbl x.path { x with aggressive = x.aggressive || x'.aggressive }
      with
      | Not_found -> Hashtbl.add tbl x.path x) xs;
    Hashtbl.to_list tbl |> map snd
end

(** spec dsl *)
type t = 
  | Or of t2 list (** [t2, .., t2] *)
  | Type (** [%imp].  Encoded as a type definition.  No allowed in [%%imp_spec] *)

and t2 = 
  | Opened of Longident.t flagged (** [opened M]. The values defined under module path [P.M] which is accessible as [M] by [open P] *)
  | Direct of (Longident.t * Path.t option) flagged
    (** [P] or [just P]. [P] is for the values defined under module [P] and [P]'s sub-modules. [just P] is for values defined just under module [P] and values defined in its sub-modules are not considered. *)
  | Aggressive of t2 (** [aggressive t2]. Even normal function arrows are considered as constraints. *)
  | Related (** [related]. The values defined under module [P] where data type defined in [P] appears in the type of the resolution target *)
  | Name of string * Re.re * t2 (** [name "rex" t2]. Constraint values only to those whose names match with the regular expression *)
  | Typeclass of Path.t option (** [typeclass]. Typeclass style resolution. The argument is None at parsing, but must be filled with Some until the resolution.
  None at parsing, but must be filled with Some until the resolution *) 
  | Deriving of Longident.t (** [deriving M]. [M] must define [M.tuple], [M.object_] and [M.poly_variant] *)

and 'a flagged = In of 'a | Just of 'a

(* static : instance space is fixed
   dynamic : instance space can be changed according to the target type
*)
let rec is_static = function
  | Opened _ -> true
  | Direct _ -> true
  | Related -> false
  | Aggressive t2 | Name (_, _, t2) -> is_static t2
  | Typeclass _ -> true
  | Deriving _ -> false
    
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
    | Typeclass _ -> "typeclass"
    | Deriving p -> Printf.sprintf "deriving %s" & Longident.to_string p
  in
  t 

let prefix = "Spec_"
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
      | Pexp_ident {txt=Lident "typeclass"} -> Typeclass None
      | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "deriving"} }, args ) ->
          begin match args with
          | ["", e] -> 
              begin match get_lid e with
              | Some lid -> Deriving lid
              | None -> errorf "deriving must take an module path" Location.format e.pexp_loc
              end
          | _ -> errorf "deriving must take just one argument"
          end
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
          | None -> errorf "%a: just requires an argument" Location.format e.pexp_loc
          end
      | Pexp_construct ({txt=lid}, None) -> In lid
      | _ ->
          errorf "%a: Illegal spec expression" Location.format e.pexp_loc
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
          `Error (`String "spec must be an OCaml expression")

let error loc = function
  | `String s -> errorf "%a: %s" Location.format loc s
  | `Failed_unmangle s -> 
      errorf "%a: Illegal spec encoding: %S" Location.format loc s
  | `Parse s ->
      errorf "%a: Spec parse failed: %S" Location.format loc s
  | `ParseExp (_, s) ->
      errorf "%a: Spec parse failed: %s" Location.format loc s

let from_ok loc = function
  | `Ok v -> v
  | `Error e -> error loc e

let from_payload = function
  | PStr s -> from_structure s
  | _ -> `Error (`String "spec must be an OCaml expression")

(* typed world *)

open Types

(** fill Typeclass None *)
let fix_typeclass _loc p = function
  | Type -> assert false
  | Or t2s ->
      let f = function
        | Typeclass None -> Typeclass (Some p)
        | Typeclass (Some _) -> assert false
        | x -> x
      in
      Or (map f t2s)
    
let from_type_decl p loc = function
  | { type_params = []
    ; type_kind = Type_variant [ { cd_id= id; cd_args = []; cd_res = None; cd_loc = loc} ]
    ; type_manifest = None } ->
      let (>>=) x f = match x with `Error e -> `Error e | `Ok v -> f v in
      fix_typeclass loc p
      & from_ok loc
      & unmangle id.Ident.name >>= from_string >>= from_expression
  | _ -> 
      errorf "%a: Illegal data type definition for __imp_spec__. [%%%%imp_spec SPEC] must be used." Location.format loc

        
(* Build an empty type env except mp module with the given module type *)
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
  
let from_module_type env mp loc mty =
  let m = new dummy_module env mp mty in
  try
    let p, td = m#lookup_type "__imp_spec__" in
    (* Add mp.Instances as the default recipes *)
    match from_type_decl p loc td with
    | Type -> assert false
    | Or t2s ->
        let default_instances =
          try
            let p = m#lookup_module "Instances" in
            [ Direct (In (Typpx.Untypeast.lident_of_path p, Some p)) ]
          with
          | Not_found -> []
        in
        `Ok (Or (t2s @ default_instances))
  with _ -> `Error (`No_imp_spec (loc, mp))

let from_module_path ~imp_loc env mp =
  let md =
    try Env.find_module mp env with _ ->
      eprintf "%a: BUG of ppx_implicits: Unbound module %a." Location.format imp_loc Path.format mp;
      assert false
  in
  from_module_type env mp md.md_loc md.md_type

let check_module env loc path =
  match 
    try Some (Env.find_module path env) with _ -> None
  with
  | None -> 
      errorf "%a: no module desc found: %a" Location.format loc Path.format path
  | Some mdecl -> mdecl

let _test_scrape_sg path env sg =
  match path with
  | Pident {Ident.name = "Pervasives"} -> ()
  | _ ->
  let lid = Typpx.Untypeast.lident_of_path path in      
  eprintf "SCRAPE SG %a@." Path.format_verbose path;
  flip iter sg & function
    | Sig_value (id, _vdesc) ->
        let lid = Ldot (lid, id.Ident.name) in
        let popt = try Some (fst (Env.lookup_value lid env)) with _ -> None in
        eprintf "  value %a  >> %a@." Ident.format_verbose id (Option.format Path.format_verbose) popt
    | Sig_module (id, _moddecl, _) ->
        eprintf "  module %a@." Ident.format_verbose id
    | Sig_type (id, _, _) -> 
        eprintf "  type %a@." Ident.format_verbose id
    | _ -> ()
    
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
      eprintf "scraping failed: %s" & Printexc.to_string e;
      raise e

let rec values_of_module ~recursive env lid path mdecl : Candidate.t list =
  let m = new dummy_module env path mdecl.md_type in
  let sg = scrape_sg path env mdecl in
  flip2 fold_right sg [] & fun sitem st -> match sitem with
  | Sig_value (id, _vdesc) ->
      let lid = Ldot (lid, Ident.name id) in
      let path = try m#lookup_value & Ident.name id with Not_found ->
        eprintf "VOM m#lookup_value %s not found@." & Ident.name id;
        assert false
      in
      begin
        try
          let type_ = (Env.find_value path env).val_type in
          let expr = Forge.Exp.(with_env env & ident path) in
          (* eprintf "    VOM: %a@." Path.format_verbose path; *)
          { Candidate.lid; path; expr; type_; aggressive= false }  :: st
        with
        | Not_found ->
            eprintf "VOM: %a but not found@." Path.format_verbose path;
            assert false
      end
  | Sig_module (id, moddecl, _) when recursive -> 
      let lid = Ldot (lid, Ident.name id) in
      let path = m#lookup_module & Ident.name id in
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

let _dump_summary env = dump_summary & Env.summary env
  
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
      let sg = scrape_sg open_ env mdecl in
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
        try
          Env.lookup_module ~load:true lid env
        with
        | Not_found -> errorf "%a: Unbound module %a." Location.format loc Longident.format lid
  in
  let mdecl = check_module env loc path in
  values_of_module ~recursive env lid path mdecl

let cand_related env _loc ty = 
  let mods = related_modules env ty in
  let lmods = flip map mods & fun p ->
    let mdecl = Env.find_module p env in
    (Typpx.Untypeast.lident_of_path p, p, mdecl)
  in
  (* CR jfuruse: values_of_module should be memoized *)
  concat & map (fun (lid,path,mdecl) -> values_of_module ~recursive:false env lid path mdecl) lmods
  
let cand_opened env loc x =
  let lid = match x with
    | Just lid -> lid
    | In lid -> lid
  in
  let opens = get_opens env in
  if !Options.debug_resolve then begin
    eprintf "debug_resolve: cand_opened opened paths@.";
    flip iter opens & eprintf "  %a@." Path.format
  end;
  let paths = 
    concat 
    & map (module_lids_in_open_path env [lid]) 
    & None :: map (fun x -> Some x) opens
  in
  if !Options.debug_resolve then begin
    eprintf "debug_resolve: cand_opened cand modules@.";
    flip iter paths & eprintf "  %a@." Path.format
  end;
  concat & map (fun path ->
    let lid = Typpx.Untypeast.lident_of_path path in
    cand_direct env loc
      (match x with
      | Just _ -> Just (lid, Some path)
      | In _ -> In (lid, Some path))) paths

(* [%imp] : 'a M.ty
   type M.__imp_spec__ must exists and it is "typeclass"
   We seek types equal to __imp_instance__ = M.__imp_spec__
*) 
let cand_typeclass env loc p_spec =
  let has_instance mp =
    let md = Env.find_module mp env in
    let m = new dummy_module env mp md.md_type in
    try
      let _, td = m#lookup_type "__imp_instance__" in
      match td with
      | { type_params = []
        ; type_manifest = Some { desc = Tconstr (p, _, _) } } when p = p_spec ->
          Some mp
          (* Some (Pdot (mp, "__imp_instance__", n)) *)
      | _ -> None
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
    | Env_module (s, id, _md) ->
        let res = find_modules s in
        begin match has_instance (Pident id) with
        | None -> res
        | Some x -> x :: res
        end
    | Env_open (s, path) ->
        let md = Env.find_module path env in
        (* Strange way to ask the correct position ... *)
        let m = new dummy_module env path md.md_type in
        fold_left (fun res -> function
          | Sig_module (id, _md, _) ->
              begin match has_instance (m#lookup_module id.Ident.name) with
              | None -> res
              | Some x -> x :: res
              end
          | _ -> res)
          (find_modules s) (scrape_sg path env md)
  in
  let paths = find_modules & Env.summary env in
  if !Options.debug_resolve then begin
    eprintf "debug_resolve: cand_typeclass cand modules@.";
    flip iter paths & eprintf "  %a@." Path.format
  end;
  concat & map (fun path ->
    let lid = Typpx.Untypeast.lident_of_path path in
    cand_direct env loc (Just (lid, Some path))) paths
    
let cand_name rex f =
  filter (fun x ->
    Re_pcre.pmatch ~rex & Longident.to_string x.Candidate.lid) & f ()

let test_cand_deriving env loc ty path vd form_check =
  let vty = vd.val_type in
  let dlabel, _dty, vty' =
    (* vty must be ~_d:Obj.t -> ty['a] *)
    (* CR jfuruse: need a check dty = Obj.t *)
    match expand_repr_desc env vty with
    | Tarrow (l, dty, vty', _) when Klabel.is_klabel l = Some `Normal -> l, dty, vty'
    | _ -> 
        errorf "@[<2>%a: %a has a bad type %a for deriving.@ It must have a constraint non optional label argument.@]" Location.format loc Path.format path Printtyp.type_scheme vty
  in
      
  let var_check path ty =
    (* a magic func must have only one type variable, generalized *)
    (* [Ctype.free_variables] also returns generic variables *)
    match gen_vars ty, Ctype.free_variables ty with
    | [v], [v'] when v == v' -> v 
    | _ ->
        errorf "@[<2>%a: %a has a bad type %a for deriving.@ It must have only one type variable and it must be generalized.@]" Location.format loc Path.format path Printtyp.type_scheme vty
  in
  let v = var_check path vty' in

  with_snapshot & fun () ->
    match Ctype.instance_list env [v; vty'] with
    | [v; vty''] ->
        Ctype.unify env ty vty'';
        form_check dlabel v vty';
        (* return a candidate expr *)
    | _ -> assert false

let cand_deriving_tuple env loc ty mlid =
  let lid = Ldot (mlid, "tuple") in
  let path, vd = 
    try Env.lookup_value lid env with Not_found -> 
      errorf "%a: %a is not defined." Location.format loc Longident.format lid
  in
  test_cand_deriving env loc ty path vd & fun dlabel v temp_ty ->
    match expand_repr_desc env v with
    | Ttuple tys ->
        (* Build [fun ~_l1:d1 .. ~_ln:dn -> M.tuple ~_d:(Obj.repr (d1,..,dn))] *)
        let obj_repr e =
          let obj_repr_path, _ = (* I trust it is Obj.repr *)
            try
              Env.lookup_value (Longident.(Ldot (Lident "Obj", "repr"))) env
            with
            | Not_found -> 
                errorf "%a: Obj.repr is required but not accessible" Location.format loc
          in
          Forge.Exp.(app (with_env env & ident obj_repr_path) ["", e])
        in
        let len = length tys in
        let nums = let rec f st = function 0 -> st | n -> f (n::st) (n-1) in f [] len in
        let ids = map (fun i -> Ident.create (Printf.sprintf "__deriving__%d" i)) nums in
        let labels = map (Printf.sprintf "_d%d") nums in
        let tpl = Forge.Exp.( tuple (map (fun id -> with_env env & ident (Path.Pident id)) ids )) in
        let e = Forge.Exp.(app (with_env env & ident path) [dlabel, obj_repr tpl]) in
        let type_ =
          fold_right2 (fun label ty st ->
            let ty' = Ctype.instance env temp_ty in
            begin match Ctype.free_variables ty' with
            | [v] -> Ctype.unify env ty v
            | _ -> assert false
            end;
            Forge.Typ.arrow ~label ty' st) labels tys ty
        in
        Some {Candidate.lid;
              path;
              expr = fold_right2 (fun id label e ->
                Forge.(Exp.fun_ ~label (Pat.var id) e)) ids labels e;
              type_;
              aggressive = false}
    | _ -> None
  
let cand_deriving env loc ty mlid =
  filter_map (fun x -> x)
    [ cand_deriving_tuple env loc ty mlid;
      (* test form_check_object_ p_object vd_object_; *)
      (* test form_check_polymorphic_variant p_polymorphic_variant vd_polymorphic_variant *) ]
  
let rec cand_static env loc : t2 -> Candidate.t list = function
  | Aggressive x ->
      map (fun x -> { x with Candidate.aggressive = true }) & cand_static env loc x
  | Opened x -> cand_opened env loc x
  | Direct x -> cand_direct env loc x
  | Name (_, rex, t2) -> cand_name rex & fun () -> cand_static env loc t2
  | Typeclass (Some p) -> cand_typeclass env loc p
  | Typeclass None -> assert false
  | spec when is_static spec -> assert false
  | _ -> assert false

let rec cand_dynamic env loc ty = function
  | Related -> cand_related env loc ty
  | Aggressive x -> map (fun x -> { x with Candidate.aggressive= true }) & cand_dynamic env loc ty x
  | Name (_, rex, t2) -> cand_name rex & fun () -> cand_dynamic env loc ty t2
  | Deriving lid -> cand_deriving env loc ty lid
  | Opened _ | Direct _ | Typeclass _ ->
      (* they are static *)
      assert false

let candidates env loc = function
  | Type -> assert false (* This should not happen *)
  | Or ts ->
      let statics, dynamics = partition is_static ts in
      let statics = concat & map (cand_static env loc) statics in
      if !Options.debug_resolve then begin
        eprintf "debug_resolve: static candidates@.";
        flip iter statics & fun x ->
          eprintf "  %a@." Pprintast.expression (Typpx.Untypeast.untype_expression x.Candidate.expr)
      end;
      let dynamics ty = concat & map (cand_dynamic env loc ty) dynamics in
      fun ty -> Candidate.uniq & statics @ dynamics ty
