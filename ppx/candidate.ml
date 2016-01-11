(*

  Instance search space specification DSL, magling to and back from
  OCaml type definitions.

*)
open Ppxx.Utils
open List

open Ppxx.Compilerlib
open Longident
open Path
open Types

open Typpx

open Spec
open Asttypes

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

let rec values_of_module ~recursive env lid path mdecl : t list =
  let m = new Spec.dummy_module env path mdecl.md_type in
  let sg = scrape_sg path env mdecl in
  flip2 fold_right sg [] & fun sitem st -> match sitem with
  | Sig_value (id, _vdesc) ->
      let lid = Ldot (lid, Ident.name id) in
      let path = try m#lookup_value & Ident.name id with Not_found ->
        !!% "VOM m#lookup_value %s not found@." & Ident.name id;
        assert false
      in
      begin
        try
          let type_ = (Env.find_value path env).val_type in
          let expr = Forge.Exp.(with_env env & ident path) in
          (* !!% "    VOM: %a@." Path.format_verbose path; *)
          { lid; path; expr; type_; aggressive= false }  :: st
        with
        | Not_found ->
            !!% "VOM: %a but not found@." Path.format_verbose path;
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

let rec dump_summary = function
  | Env.Env_empty -> ()
  | Env_value (s, _, _)
  | Env_extension (s, _, _)
  | Env_modtype (s, _, _)
  | Env_class (s, _, _)
  | Env_cltype (s, _, _)
  | Env_functor_arg (s, _) -> dump_summary s
  | Env_type (s, id, _) -> !!% "type %a@." Ident.format id; dump_summary s
  | Env_module (s, id, _) -> !!% "module %a@." Ident.format id; dump_summary s
  | Env_open (s, path) -> !!% "open %a@." Path.format path; dump_summary s

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
      (*  !!% "open %a@." Path.format open_; *)
      let mdecl = Env.find_module open_ env in (* It should succeed *)
      let sg = scrape_sg open_ env mdecl in
      let env = Env.open_signature Fresh open_ sg Env.empty in
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
    !!% "debug_resolve: cand_opened opened paths@.";
    flip iter opens & !!% "  %a@." Path.format
  end;
  let paths = 
    concat 
    & map (module_lids_in_open_path env [lid]) 
    & None :: map (fun x -> Some x) opens
  in
  if !Options.debug_resolve then begin
    !!% "debug_resolve: cand_opened cand modules@.";
    flip iter paths & !!% "  %a@." Path.format
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
    !!% "debug_resolve: cand_typeclass cand modules@.";
    flip iter paths & !!% "  %a@." Path.format
  end;
  concat & map (fun path ->
    let lid = Typpx.Untypeast.lident_of_path path in
    cand_direct env loc (Just (lid, Some path))) paths
    
let cand_name rex f =
  filter (fun x ->
    Re_pcre.pmatch ~rex & Longident.to_string x.lid) & f ()

module Deriving = struct
  let obj_repr env loc e =
    let obj_repr_path, _ = (* I trust it is Obj.repr *)
      try
        Env.lookup_value (Longident.(Ldot (Lident "Obj", "repr"))) env
      with
      | Not_found -> 
          errorf "%a: Obj.repr is required but not accessible" Location.format loc
    in
    Forge.Exp.(app (with_env env & ident obj_repr_path) ["", e])
  
  let exit_then_none f = try f () with Exit -> None
  
  (* vd.val_type must have the form ~_d:Obj.t list -> ty['a] *)
  let test_cand_deriving env loc ty lid =
    let path, vd =
      (* CR jfuruse: this can be called more than once for one instance resolution *)
      try Env.lookup_value lid env with Not_found -> 
        warnf "%a: %a is not defined." Location.format loc Longident.format lid;
        raise Exit
    in
    let vty = vd.val_type in
    let dlabel, _dty, vty' =
      (* CR jfuruse: need a check dty = Obj.t list *)
      match expand_repr_desc env vty with
      | Tarrow (l, dty, vty', _) when Klabel.is_klabel l = Some `Normal -> l, dty, vty'
      | _ -> 
          errorf "@[<2>%a: %a has a bad type %a for deriving.@ It must have a constraint non optional label argument.@]"
            Location.format loc
            Path.format path
            Printtyp.type_scheme vty
    in
        
    let v =
      (* a magic func must have only one type variable, generalized *)
      (* [Ctype.free_variables] also returns generic variables *)
      match gen_vars vty', Ctype.free_variables vty' with
      | [v], [v'] when v == v' -> v 
      | _ ->
          errorf "@[<2>%a: %a has a bad type %a for deriving.@ It must have only one type variable and it must be generalized.@]"
            Location.format loc
            Path.format path
            Printtyp.type_scheme vty'
    in
  
    match Ctype.instance_list env [v; vty'] with
    | [v; vty''] ->
        with_snapshot & fun () ->
          (* tricky. by [repr v], we can keep what we want even after
             the unification is undone *)
          Ctype.unify env ty vty'';
          (path, dlabel, Ctype.repr v, vty')
    | _ -> assert false
  
  type 'a field = {
    value : 'a;
    ident : Ident.t;
    dlabel : label;
  }
  
  let make_fields vs =
    mapi (fun i v ->
      { value= v;
        ident = Ident.create (Printf.sprintf "__deriving__%d" i);
        dlabel = Printf.sprintf "_d%d" i }) vs
  
  (* konstraint abstractions *)    
  let kabs fields e =
    let open Forge in
    fold_right (fun {ident=id; dlabel} e ->
      Exp.fun_ ~label:dlabel (Pat.var id) e) fields e
  
  let build_type getty env template_ty fields ty_return =
    let open Ctype in
    (* _d1:ty1 -> .. -> _dn:tyn -> ty, removing 0 ary constructors *)
    fold_right (fun { value; dlabel } st ->
      let ty = getty value in
      (* no need to undo the unification since template_ty has no free tyvar but one generalized tvar *)
      let template_ty' = instance env template_ty in
      begin match free_variables template_ty' with
      | [v] -> unify env ty v
      | _ -> assert false
      end;
      Forge.Typ.arrow ~label:dlabel template_ty' st) fields ty_return
  
  let cand_deriving_tuple env loc ty mlid =
    exit_then_none & fun () ->
      let lid = Ldot (mlid, "tuple") in
      let path, dlabel, v, template_ty = test_cand_deriving env loc ty lid in
      match expand_repr_desc env v with
      | Ttuple tys ->
          (* Build [fun ~_l1:d1 .. ~_ln:dn -> M.tuple ~_d:(Obj.repr (d1,..,dn))] *)
          let fields = make_fields tys in
          let ds =
            let open Forge.Exp in
            list env & map (fun {ident=id} -> obj_repr env loc & with_env env & ident (Path.Pident id)) fields
          in
          let e = Forge.Exp.(app (with_env env & ident path) [dlabel, ds]) in
          let expr = kabs fields e in
          let type_ = build_type (fun ty -> ty) env template_ty fields ty in
          Some { lid; path; expr; type_; aggressive = false}
      | _ -> None
    
    
  let cand_deriving_polymorphic_variant env loc ty mlid =
    exit_then_none & fun () ->
      let lid = Ldot (mlid, "polymorphic_variant") in
      let path, dlabel, v, template_ty = test_cand_deriving env loc ty lid in
      match expand_repr_desc env v with
      | Tvariant rdesc ->
          let rdesc = Btype.row_repr rdesc in
  (* I guess we do not need it
          if not rdesc.row_closed then begin
            warnf "@[<2>%a: %a cannot be used as an instance since the raw type %a is open.@]"
              Location.format loc
              Path.format path
              Printtyp.type_scheme v;
            raise Exit
          end;
  *)
          (* Note: Not all fields have types *)
          let fields = make_fields & map (function
              | (l, Rpresent tyo) -> l, tyo
              | (l, _) ->
                  warnf "@[<2>%a: %a cannot be used as an instance since the raw type %a has inappropriate field type %s@]"
                    Location.format loc
                    Path.format path
                    Printtyp.type_scheme v
                    l;
                  raise Exit) rdesc.row_fields
          in
          let ds =
            let open Forge.Exp in
            list env & map (fun {value=(l,tyo); ident=id} ->
              let hash = Btype.hash_variant l in
              (* CR jfuruse: of course we should have Forge.Exp.{string,int} *)
              tuple [ untyped (Ppxx.Helper.Exp.string l);
                      untyped (Ppxx.Helper.Exp.int hash);
                      match tyo with
                      | None -> none env
                      | Some _ ->
                          some env & obj_repr env loc & with_env env & ident & Path.Pident id ])
                     fields
          in
          let e = Forge.Exp.(app (with_env env & ident path) [dlabel, ds]) in
          (* from here, we discard 0 ary constructors *)
          let fields = filter_map (function
            | {value=(_, None)} -> None
            | {value=(l, Some ty); ident; dlabel} -> Some {value=(l,ty); ident; dlabel}) fields
          in
          let expr = kabs fields e in
          let type_ = build_type snd env template_ty fields ty in
          Format.eprintf "candidate deriving PV: %a@."
            Printtyp.type_scheme type_;
          Some { lid; path; expr; type_; aggressive = false}
    | _ -> None
    
    
  (* 
  Very good tutorial about OCaml Object internals:
  
  http://ambassadortothecomputers.blogspot.jp/2010/03/inside-ocaml-objects.html 
  *)
  
  let cand_deriving_object env loc ty mlid =
    let open Btype in
    let open Ctype in
    exit_then_none & fun () ->
      let lid = Ldot (mlid, "object_") in
      let path, dlabel, v, template_ty = test_cand_deriving env loc ty lid in
      match expand_repr_desc env v with
      | Tobject (fields, _) ->
          let fields, _tvar (*probably?*) = flatten_fields fields in
          (* method types are wrapped by Tpoly. See Pexp_send case of typecore.ml, type_expect_ *)
          let fix_ty f ty = match repr_desc ty with
            | Tpoly(ty, []) -> instance env ty
            | Tpoly(_, _) ->
                (* I have no idea how to handle polymoprhic methods here *)
                warnf "@[<2>%a: %a cannot be auto-derived since it has a polymorphic method %s:%a.@]"
                  Location.format loc
                  Path.format path
                  f
                  Printtyp.type_scheme ty;
                raise Exit
            | Tvar _ ->
                let ty' = newvar () in
                unify env (instance_def ty) (newty(Tpoly(ty',[])));
                ty'
            | _ -> assert false
          in
          let fields = map (fun (l,fk,ty) -> (l, field_kind_repr fk, fix_ty l ty)) fields in
          if exists (fun (_,fk,_) -> fk <> Fpresent) fields then begin
            warnf "@[<2>%a: %a cannot be used as an instance since the object type %a has a non-present method.@]"
              Location.format loc
              Path.format path
              Printtyp.type_scheme v;
            raise Exit
          end;
          let fields = make_fields & map (fun (l, _, ty) -> (l,ty)) fields in
          let ds =
            let open Forge.Exp in
            list env & map (fun {value=(l,_ty); ident=id} ->
              let hash = Obj.magic & CamlinternalOO.public_method_label l in
              (* CR jfuruse: of course we should have Forge.Exp.{string,int} *)
              tuple [ untyped (Ppxx.Helper.Exp.string l);
                      untyped (Ppxx.Helper.Exp.int hash);
                      obj_repr env loc & with_env env & ident & Path.Pident id ])
                     fields
          in
          let e = Forge.Exp.(app (with_env env & ident path) [dlabel, ds]) in
          let expr = kabs fields e in
          let type_ = build_type snd env template_ty fields ty in
          Format.eprintf "candidate deriving PV: %a@."
            Printtyp.type_scheme type_;
          Some { lid; path; expr; type_; aggressive = false}
    | _ -> None
    
    
  let cand_deriving env loc ty mlid =
    filter_map (fun x -> x)
      [ cand_deriving_tuple env loc ty mlid;
        cand_deriving_polymorphic_variant env loc ty mlid;
        cand_deriving_object env loc ty mlid;
      ]
end
  
let rec cand_static env loc : t2 -> t list = function
  | Aggressive x ->
      map (fun x -> { x with aggressive = true }) & cand_static env loc x
  | Opened x -> cand_opened env loc x
  | Direct x -> cand_direct env loc x
  | Name (_, rex, t2) -> cand_name rex & fun () -> cand_static env loc t2
  | Typeclass (Some p) -> cand_typeclass env loc p
  | Typeclass None -> assert false
  | spec when is_static spec -> assert false
  | _ -> assert false

let rec cand_dynamic env loc ty = function
  | Related -> cand_related env loc ty
  | Aggressive x -> map (fun x -> { x with aggressive= true }) & cand_dynamic env loc ty x
  | Name (_, rex, t2) -> cand_name rex & fun () -> cand_dynamic env loc ty t2
  | Deriving lid -> Deriving.cand_deriving env loc ty lid
  | Opened _ | Direct _ | Typeclass _ ->
      (* they are static *)
      assert false

let candidates env loc = function
  | Type -> assert false (* This should not happen *)
  | Or ts ->
      let statics, dynamics = partition is_static ts in
      let statics = concat & map (cand_static env loc) statics in
      if !Options.debug_resolve then begin
        !!% "debug_resolve: static candidates@.";
        flip iter statics & fun x ->
          !!% "  %a@." Pprintast.expression (Typpx.Untypeast.untype_expression x.expr)
      end;
      let dynamics ty = concat & map (cand_dynamic env loc ty) dynamics in
      fun ty -> uniq & statics @ dynamics ty
