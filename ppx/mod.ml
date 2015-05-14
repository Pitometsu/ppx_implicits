open Utils
open Typedtree
open Ppxx
open Longident (* has flatten *)
open List (* has flatten *)

let warn f = 
  Format.eprintf "@[<2>Warning:@ ";
  f ();
  Format.eprintf "@]@.";

module Types = struct
  include Types
  open Btype
  open Ctype
  let repr_desc ty = (repr ty).desc
  let expand_repr_desc env ty = (repr & expand_head env ty).desc

  let with_snapshot f =
    let snapshot = snapshot () in
    let res = protect f in
    backtrack snapshot;
    unprotect res

  let is_constr env ty = match expand_repr_desc env ty with
    | Tconstr (p, tys, _) -> Some (p, tys)
    | _ -> None
  
  let is_option_type env ty = match is_constr env ty with
    | Some (po, [ty]) when po = Predef.path_option -> Some ty
    | _ -> None

  let gen_vars ty =
    flip filter (Ctype.free_variables ty) & fun ty ->
      ty.level = Btype.generic_level
end

open Types

(* XXX.IMP.t *)
let is_implicit_path (p, lid) = 
  let open Path in 
  let open Longident in 
  match p, lid with      
  | Pdot (p, "__imp__", _), Lident "__imp__" -> Some (p, None)
  | Pdot (p, "__imp__", _), Ldot (l, "__imp__") -> Some (p, Some l)
  | _ -> None

let is_constraint_label l =
  let len = String.length l in
  len >= 2 && String.unsafe_get l 0 = '_'

(* Oops, there is no good exposed API to compare a module type
   and a packed module type. 
*)

let rec extract_constraint_labels env ty = 
  let ty = Ctype.expand_head env ty in
  match repr_desc ty with
  | Tarrow(l, ty1, ty2, _) when is_constraint_label l ->
      let cs, ty = extract_constraint_labels env ty2 in
      (l,ty1)::cs, ty
  | _ -> [], ty

let rec get_candidates env lid (* scan module lid *) mty (* scan module mty *) =
  let sg = 
    try
      match Env.scrape_alias env @@ Mtype.scrape env mty with
      | Mty_signature sg -> sg
      | _ -> assert false
    with
    | e -> 
        Format.eprintf "scraping failed: %s" & Printexc.to_string e;
        raise e
  in
  flip2 fold_right sg [] & fun sitem st -> match sitem with
    | Sig_value (id, vdesc) when id.name <> "__imp__" -> 
        let lid = Ldot (lid, Ident.name id) in
        begin try
          let path, _vdesc = Env.lookup_value lid env in
          (lid, path, vdesc) :: st
        with
        | Not_found ->
            warn (fun () -> 
              Format.eprintf "%%imp instance %a is not accessible in the current scope therefore ignored." Longident.format lid);
            st
        end
    | Sig_module (id, moddecl, _) -> 
        let lid = Ldot (lid, Ident.name id) in
        get_candidates env lid moddecl.Types.md_type @ st
    | _ -> st

let rec resolve env cands : ((Path.t * type_expr) list * type_expr) list -> expression list list = function
  | [] -> [[]]
  | (trace,ty)::tr_tys ->
      concat & flip map cands & fun (lid,path,vdesc) ->
         match
           try Some (assoc path trace) with _ -> None
         with
         | Some ty' when not & Tysize.(lt (size ty) (size ty' )) ->
             (* recursive call and the type size is not strictly decreasing *)
             Format.eprintf "Non decreasing %%imp recursive dependency: %a : %a  =>  %a@." 
               Path.format path Printtyp.type_expr ty' Printtyp.type_expr ty;
             []
         | _ ->
             let trace' = (path, ty) :: trace in (* CR jfuruse: Older binding is no longer useful. Replace instead of add? *)

             let ity = Ctype.instance env ty in
             let ivty = Ctype.instance env vdesc.val_type in
        
             let cs, ivty = extract_constraint_labels env ivty in

             Format.eprintf "Got:@.";
             flip iter cs (fun (l,ty) ->
               Format.eprintf "  %s:%a ->@." l Printtyp.type_expr ty);
             Format.eprintf "  %a@." Printtyp.type_expr ivty;

             with_snapshot & fun () ->
               try

                 Format.eprintf "Checking %a <> %a@."
                 Printtyp.type_expr ity
                 Printtyp.type_expr ivty;

                 ignore & Ctype.unify env ity ivty;
                 let tr_tys = map (fun (_,ty) -> (trace',ty)) cs @ tr_tys in
                 flip map (resolve env cands tr_tys) & fun res ->
                   let rec app res cs = match res, cs with
                     | res, [] -> res, []
                     | r::res, (l,_)::cs ->
                         let res, args = app res cs in
                         res, (l,r)::args
                     | _ -> assert false
                   in
                   let res, args = app res cs in
                   Forge.Exp.(app (ident lid path) args) :: res
               with
               | _ -> []
          
let is_imp e = 
  let open Parsetree in
  let get_modules sitem = match sitem.pstr_desc with
    | Pstr_eval (e, []) ->
        let get_module e = match e.pexp_desc with
          | Pexp_construct ({txt=lid}, None) -> lid
          | _ -> assert false
        in
        begin match e.pexp_desc with
        | Pexp_tuple es -> Some (map get_module es)
        | _ -> Some [get_module e]
        end
    | _ -> assert false (* illegal imp *)
  in
  let imps = 
    flip map e.exp_attributes (function 
      | {txt="imp"}, PStr [sitem] -> 
          Option.map (fun x -> `Imp1 x) & get_modules sitem
      | {txt="imp"}, _ -> assert false (* illegal imp *)

      | {txt="imp2"}, PStr [sitem] ->
          Option.map (fun x -> `Imp2 x) & get_modules sitem
      | {txt="imp2"}, _ -> assert false (* illegal imp *)

      | {txt="imp3"}, PStr [] -> Some `Imp3
      | {txt="imp3"}, _ -> assert false (* illegal imp *)

      | _ -> None)
  in
  match flip filter imps & function
    | Some _ -> true
    | None -> false
  with
  | [] -> None
  | [Some x] -> Some x
  | _ -> assert false (* multiple *)
  
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

let lids_in_open_path env lids = function
  | None -> 
      flip filter_map lids (fun lid ->
        try
          let path = Env.lookup_module ~load:false (*?*) lid env in
(*
          Format.eprintf "CURRENT %a@." Path.format path;
*)
          Some path
        with
        | _ -> None)
  | Some (Path.Pident id) when Ident.name id = "Pervasives" && Ident.persistent id  -> 
      (* We assume Pervasives has no instances *)
      []
  | Some open_ ->
(*
      Format.eprintf "open %a@." Path.format open_;
*)
      let mdecl = Env.find_module open_ env in (* It should succeed *)
      match Mtype.scrape env mdecl.md_type with
      | Mty_signature sg ->
          let env = Env.open_signature Asttypes.Fresh open_ sg env in
          flip filter_map lids (fun lid ->
            try
              let p = Env.lookup_module ~load:false (*?*) lid env in
(*
              Format.eprintf "%a %a@." Path.format open_ Path.format p;
*)
              Some p
            with
            | _ -> None)
      | _ -> assert false
      
let lids_in_open_paths env lids opens =
  concat_map (lids_in_open_path env lids) opens

(* Create a type which can be unified only with itself *)
let create_uniq_type =
  let cntr = ref 0 in
  fun () -> 
    incr cntr;
    Ctype.( newty ( Tconstr ( Pident (Ident.create & "*uniq*" ^ string_of_int !cntr), [], ref Mnil ) ) )

let close_gen_vars ty =
  List.iter (fun gv ->
    match repr_desc gv with
    | Tvar _ -> Ctype.unify Env.empty gv (create_uniq_type ())
    | Tunivar _ -> ()
    | _ -> assert false) & gen_vars ty

let exclude_gen_vars loc ty =
  if gen_vars ty <> [] then
    errorf "%a: overloaded value has a generalized type: %a" Location.print_loc loc Printtyp.type_scheme ty

let is_imp_option_type env ty = match is_option_type env ty with
  | None -> None
  | Some ty ->
      match expand_repr_desc env ty with
      | Tconstr (p, _, _) ->
          begin match p with
          | Pident {name = "__imp__"} -> Some ty
          | Pdot (_, "__imp__", _) -> Some ty
          | _ -> None
          end
      | _ -> None

let resolve env cands ty loc = 
  match resolve env cands [([],ty)] with
  | [] -> errorf "%a: no instance found for %a" Location.print_loc loc Printtyp.type_expr ty;
  | [[e]] -> e
  | _ -> errorf  "%a: overloaded type has a too ambiguous type: %a" Location.print_loc loc Printtyp.type_expr ty

let forge1 lids env loc ty =
  exclude_gen_vars loc ty;
  let cands = flatten & flip map lids & fun lid ->
    match 
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
        | Some mdecl -> get_candidates env lid mdecl.md_type
  in
  resolve env cands ty loc

let forge2 lids env loc ty =
  exclude_gen_vars loc ty;

  let opens = get_opens & Env.summary env in
(*
  flip iter opens (Format.eprintf "open %a@." Path.format);
*)

  let paths = sort_uniq compare & lids_in_open_paths env lids (None :: map (fun x -> Some x) opens) in

Format.eprintf "forge2@.";
(*
  iter (fun p -> Format.eprintf "found %a@." Path.format p) paths;
*)

  let cands = flatten & flip map paths & fun path ->
    match 
      try Some (Env.find_module path env) with _ -> None
    with
    | None -> 
        errorf "%a: no module desc found: %a" Location.print_loc loc Path.format path
    | Some mdecl -> get_candidates env (Untypeast.lident_of_path path) mdecl.md_type
  in
  iter (fun (_l,p,_vd) -> Format.eprintf "cand: %a@." Path.format p) cands;

  resolve env cands ty loc

let forge3 env loc ty = with_snapshot & fun () ->
  close_gen_vars ty;

  (* Get the M of type (...) ...M.name *)
  let n = match expand_repr_desc env ty with
    | Tconstr (p, _, _) ->
        begin match p with
        | Path.Pdot(p, _ (* __imp__ *), _) ->
            begin match p with
            | Pident id -> Ident.name id
            | Pdot (_, n, _) -> n
            | _ -> assert false
            end
        | _ -> assert false
        end
    | _ -> assert false
  in

  let opens = get_opens & Env.summary env in
(*
  flip iter opens (Format.eprintf "open %a@." Path.format);
*)

  let paths = sort_uniq compare & lids_in_open_paths env [Lident n] (None :: map (fun x -> Some x) opens) in
(*
  iter (fun p -> Format.eprintf "found %a@." Path.format p) paths;
*)

  let cands = flatten & flip map paths & fun path ->
    match 
      try Some (Env.find_module path env) with _ -> None
    with
    | None -> 
        errorf "%a: no module desc found: %a" Location.print_loc loc Path.format path
    | Some mdecl -> get_candidates env (Untypeast.lident_of_path path) mdecl.md_type
  in
  resolve env cands ty loc

let resolve_arg loc env = function
  (* (l, None, Optional) means not applied *)
  | (l, Some e, Optional as a) when Btype.is_optional l ->
      begin match e.exp_desc with
      | Texp_construct ({Location.txt=Lident "None"}, _, []) ->
          begin match is_imp_option_type env e.exp_type with
          | None -> a
          | Some ty ->
              (l, Some (Forge.Exp.some (forge3 env loc ty)), Optional)
          end
      | _ -> a
      end
  | a -> a

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let app = function
    | ({ exp_desc= Texp_apply (f, args) } as e) ->
        { e with
          exp_desc= Texp_apply (f, map (resolve_arg f.exp_loc e.exp_env) args) }
    | e -> e

  let generalized_imp_args = ref []

  let enter_pattern p = 
    begin match is_imp_option_type p.pat_env p.pat_type with
    | None -> ()
    | Some ty ->
       (* Trouble:
            let f (x : a) = ... 
              let f ?imp:(i : 'a Show.__imp__) ...
        *)
        let tvars = Ctype.free_variables ty in
        let gtvars = filter (fun ty -> ty.level = Btype.generic_level) tvars in
        if tvars = gtvars then
          generalized_imp_args := (p,ty) :: !generalized_imp_args
    end;
    p

  let leave_pattern p = 
    begin match !generalized_imp_args with
    | (p',_) :: xs when p == p' -> generalized_imp_args := xs
    | _ -> ()
    end;
    p
   
  let enter_expression e = match is_imp e with
    | None -> app e

    | Some (`Imp1 lids) ->
        forge1 lids e.exp_env e.exp_loc e.exp_type

    | Some (`Imp2 lids) ->
        forge2 lids e.exp_env e.exp_loc e.exp_type

    | Some `Imp3 ->
        forge3 e.exp_env e.exp_loc e.exp_type
end

module Map = TypedtreeMap.MakeMap(MapArg)
