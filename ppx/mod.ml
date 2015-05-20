open Utils
open Typedtree
open Ppxx
open Longident (* has flatten *)
open List (* has flatten *)
open Format

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
        eprintf "scraping failed: %s" & Printexc.to_string e;
        raise e
  in
  flip2 fold_right sg [] & fun sitem st -> match sitem with
    | Sig_value (id, _vdesc) when id.name <> "__imp__" -> 
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

let rec resolve env cands : ((Path.t * type_expr) list * type_expr) list -> expression list list = function
  | [] -> [[]]
  | (trace,ty)::tr_tys ->
      concat & flip map cands & fun (lid,path,vdesc) ->
         match
           try Some (assoc path trace) with _ -> None
         with
         | Some ty' when not & Tysize.(lt (size ty) (size ty' )) ->
             (* recursive call and the type size is not strictly decreasing *)
             if !Ppxx.debug_resolve then 
               eprintf "@[<2>Non decreasing %%imp recursive dependency:@ %a : %a  =>  %a@]@." 
                 Path.format path Printtyp.type_expr ty' Printtyp.type_expr ty;
             []
         | _ ->
             let trace' = (path, ty) :: trace in (* CR jfuruse: Older binding is no longer useful. Replace instead of add? *)

             let ity = Ctype.instance env ty in
             let ivty = Ctype.instance env vdesc.val_type in
        
             let cs, ivty = extract_constraint_labels env ivty in

(*
             eprintf "Got:@.";
             flip iter cs (fun (l,ty) ->
               eprintf "  %s:%a ->@." l Printtyp.type_expr ty);
             eprintf "  %a@." Printtyp.type_expr ivty;
*)
             
             with_snapshot & fun () ->
               try

                 if !Ppxx.debug_unif then
                   eprintf "Checking %a <> %a ..."
                     Printtyp.type_expr ity
                     Printtyp.type_expr ivty;

                 begin match protect & fun () -> Ctype.unify env ity ivty with
                 | `Ok _ ->
                     if !Ppxx.debug_unif then
                       eprintf " ok: %a@." Printtyp.type_expr ity
                 | `Error (Ctype.Unify trace as e) ->
                     if !Ppxx.debug_unif then begin
                       eprintf " no@.";
                       eprintf "   Reason: @[%a@]@."
                         (fun ppf trace -> Printtyp.report_unification_error ppf
                           env trace
                           (fun ppf ->
                             fprintf ppf "Hmmm ")
                           (fun ppf ->
                             fprintf ppf "with"))
                         trace;
                     end;
                     raise e
                 | `Error e -> raise e
                 end;
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
  let imps = 
    flip map e.exp_attributes (function 
      | {txt="imp"}, payload -> Some (Policy.from_payload payload)
      | _ -> None)
  in
  match flip filter imps & function Some _ -> true | None -> false with
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
          eprintf "CURRENT %a@." Path.format path;
          *)
          Some path
        with
        | _ -> None)
  | Some (Path.Pident id) when Ident.name id = "Pervasives" && Ident.persistent id  -> 
      (* We assume Pervasives has no instances *)
      []
  | Some open_ ->
      (*
      eprintf "open %a@." Path.format open_;
      *)
      let mdecl = Env.find_module open_ env in (* It should succeed *)
      match Mtype.scrape env mdecl.md_type with
      | Mty_signature sg ->
          let env = Env.open_signature Asttypes.Fresh open_ sg env in
          flip filter_map lids (fun lid ->
            try
              let p = Env.lookup_module ~load:false (*?*) lid env in
              (*
              eprintf "%a %a@." Path.format open_ Path.format p;
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
    (* Ident.create is not good. Unifying this data type ident with
       a tvar may cause "escaping the scope" errors
    *)
    Ctype.newty ( Tconstr ( Pident (Ident.create_persistent & "*uniq*" ^ string_of_int !cntr), [], ref Mnil ) )

let close_gen_vars ty =
  List.iter (fun gv ->
    match repr_desc gv with
    | Tvar _ ->
        Ctype.unify Env.empty gv (create_uniq_type ());
        (* eprintf "Closing %a@." Printtyp.type_expr gv *)
    | Tunivar _ -> ()
    | _ -> assert false) & gen_vars ty
    
let exclude_gen_vars loc ty =
  if gen_vars ty <> [] then
    errorf "%a: overloaded value has a generalized type: %a" Location.print_loc loc Printtyp.type_scheme ty

let is_imp_type env ty = 
  match expand_repr_desc env ty with
  | Tconstr (p, _, _) ->
      begin match p with
      | Pident {name = "__imp__"} -> Some (None, ty)
      | Pdot (p, "__imp__", _) -> Some (Some p, ty)
      | _ -> None
      end
  | _ -> None

let is_imp_option_type env ty = match is_option_type env ty with
  | None -> None
  | Some ty -> is_imp_type env ty

let resolve policy env loc ty = with_snapshot & fun () ->
  if !Ppxx.debug_resolve then eprintf "@.RESOLVE: %a@." Location.print_loc loc;
  let cands = Policy.candidates loc env policy in
  close_gen_vars ty;
  if !Ppxx.debug_resolve then
    iter (fun (_lid, path, vdesc) ->
      eprintf "candidate @[<2>%a@ : %a@]@." Path.format path Printtyp.type_scheme vdesc.val_type) cands;
  match resolve env cands [([],ty)] with
  | [] -> errorf "@[<2>%a:@ no instance found for@ @[%a@]@]" Location.print_loc loc Printtyp.type_expr ty;
  | [[e]] -> e
  | _ -> errorf  "@[<2>%a:@ overloaded type has a too ambiguous type:@ @[%a@]@]" Location.print_loc loc Printtyp.type_expr ty

let resolve_imp policy env loc ty =
  (* fix the policy if ty = __imp__ *)
  let policy = match policy with
    | Policy.Type ->
        begin match is_imp_type env ty with
        | None -> assert false
        | Some (Some mp, _ty) ->
            (* CR jfuruse: dupe at resolve_arg *)
            let md = Env.find_module mp env in (* CR jfuruse: Error *)
            begin match Policy.from_module_type env md.md_type with
            | None -> assert false (* error *)
            | Some policy -> policy
            end
        | Some _ -> assert false
        end
    | _ -> policy
  in
  resolve policy env loc ty

(* ?l:None  where (None : X...Y.__imp__ option) has a special rule *) 
let resolve_arg loc env = function
  (* (l, None, Optional) means not applied *)
  | (l, Some e, Optional as a) when Btype.is_optional l ->
      begin match e.exp_desc with
      | Texp_construct ({Location.txt=Lident "None"}, _, []) ->
          begin match is_imp_option_type env e.exp_type with
          | None -> a
          | Some (Some mp, _ty) ->
              let md = Env.find_module mp env in (* CR jfuruse: Error *)
              begin match Policy.from_module_type env md.md_type with
              | None -> 
                  eprintf "policy not found in %a@." Printtyp.modtype md.md_type;
                  assert false (* error *)
              | Some policy -> (l, Some (resolve policy env loc e.exp_type), Optional)
              end
          | _ -> assert false (* error *)
          end
      | _ -> a
      end
  | a -> a

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let generalized_imp_args = ref []

  let enter_pattern p = 
    begin match is_imp_option_type p.pat_env p.pat_type with
    | None -> ()
    | Some (_, ty) ->
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

  let enter_expression e = match e.exp_desc with
    | Texp_apply (f, args) ->
        { e with
          exp_desc= Texp_apply (f, map (resolve_arg f.exp_loc e.exp_env) args) }

(*
    | Texp_function (_, cases, _) ->
        
        scopes := map (fun {c_lhs; c_guard; c_rhs} ->
          let es = match c_guard with
            | None -> [c_rhs]
            | Some e -> [e; c_rhs]
          in
          Hashtbl.replace scopes e.exp_loc (e, c_lhs)
*)
          
    | _ -> match is_imp e with
    | None -> e

    | Some (policy, _loc) -> resolve_imp policy e.exp_env e.exp_loc e.exp_type
end

module Map = TypedtreeMap.MakeMap(MapArg)
