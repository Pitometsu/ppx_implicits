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

let is_constraint_label l =
  let len = String.length l in
  if len >= 2 && String.unsafe_get l 0 = '_' then Some `Normal
  else if len >= 3 && String.sub l 0 2 = "?_" then Some `Optional
  else None

(* Constraint labels must precede the other arguments *)
let rec extract_constraint_labels env ty = 
  let ty = Ctype.expand_head env ty in
  match repr_desc ty with
  | Tarrow(l, ty1, ty2, _) when is_constraint_label l <> None ->
      let cs, ty = extract_constraint_labels env ty2 in
      (l,ty1)::cs, ty
  | _ -> [], ty

let rec extract_constraint_labels_aggressively env ty =
  let ty = Ctype.expand_head env ty in
  match repr_desc ty with
  | Tarrow(l, ty1, ty2, _) ->
      ([], ty)
      :: map
        (fun (cs, ty) -> (l,ty1)::cs, ty)
        (extract_constraint_labels_aggressively env ty2)
  | _ -> [[], ty]
    
let is_imp e = 
  let imps = 
    flip map e.exp_attributes (function 
      | {txt="imp"}, payload -> Some (Policy.from_payload payload)
      | _ -> None)
  in
  match flip filter imps & function Some _ -> true | None -> false with
  | [] -> None
  | [Some x] -> Some (Policy.from_ok e.exp_loc x)
  | _ -> assert false (* multiple *)
  
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
    
(* derived candidates *)
let derived_candidates = ref []
          
let rec resolve env get_cands : ((Path.t * type_expr) list * type_expr) list -> expression list list = function
  | [] -> [[]]
  | (trace,ty)::tr_tys ->
      let cands =
        let cs = get_cands ty in
        concat & map (fun (lid, path, vdesc, aggressive) ->
          if not aggressive then [(lid, path, vdesc, extract_constraint_labels env vdesc.val_type)]
          else map (fun cs_ty -> (lid, path, vdesc, cs_ty)) & 
            extract_constraint_labels_aggressively env vdesc.val_type) cs
      in
      concat & flip map cands & fun (lid,path,_vdesc,(cs,vty)) ->
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
        
             let ivty, cs =
               match Ctype.instance_list env (vty::map snd cs) with
               | [] -> assert false
               | ivty :: ictys ->
                   ivty, map2 (fun (l,_) icty -> (l,icty)) cs ictys
             in

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
                 flip map (resolve env get_cands tr_tys) & fun res ->
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

let resolve policy env loc ty = with_snapshot & fun () ->
  if !Ppxx.debug_resolve then eprintf "@.RESOLVE: %a@." Location.print_loc loc;
  close_gen_vars ty;
  let get_cands =
    let f = Policy.candidates env loc policy in
    fun ty -> Policy.uniq & f ty @ map snd !derived_candidates
  in
  match resolve env get_cands [([],ty)] with
  | [] -> errorf "@[<2>%a:@ no instance found for@ @[%a@]@]" Location.print_loc loc Printtyp.type_expr ty;
  | [[e]] -> e
  | _ -> errorf  "@[<2>%a:@ overloaded type has a too ambiguous type:@ @[%a@]@]" Location.print_loc loc Printtyp.type_expr ty

(* get the policy for [%imp] from its type *)
let imp_type_policy env loc ty =
  match expand_repr_desc env ty with
  | Tconstr (p, _, _) -> 
      begin match p with
      | Pident _ ->
          (* __imp_policy__ must exit *)
          let _p, td = 
            try Env.lookup_type (Lident "__imp_policy__") env with
            | _ -> errorf "%a: Current module has no implicit policy declaration [%%%%imp_policy POLICY]" Location.print_loc loc
          in
          `Ok (Policy.from_type_decl td.type_loc td)
      | Pdot (mp, _, _) -> 
          (* mp.__imp_policy__ must exist *)
          `Ok (Policy.from_module_path env mp)
      | _ -> assert false (* F(X) *)
      end
  | _ -> `Error `Strange_type
    
let resolve_imp policy env loc ty =
  let policy = match policy with
    | Policy.Type ->
        (* fix the policy for [%imp] *)
        begin match imp_type_policy env loc ty with
        | `Error `Strange_type ->
            errorf "%a: [%%%%imp] has a bad type: %a" 
              Location.print_loc loc
              Printtyp.type_expr ty
        | `Ok p -> p
        end
    | _ -> policy
  in
  resolve policy env loc ty

(* ?_l:None  where (None : X...Y.name option) has a special rule *) 
let resolve_arg loc env a = match a with
  (* (l, None, Optional) means not applied *)
  | (l, Some e, Optional) when is_constraint_label l = Some `Optional ->
      begin match e.exp_desc with
      | Texp_construct ({Location.txt=Lident "None"}, _, []) ->
          begin match is_option_type env e.exp_type with
          | None -> assert false
          | Some ty ->
              begin match imp_type_policy env loc ty with
              | `Error `Strange_type -> a (* Think abount derived! *)
              | `Ok policy ->
                  (l, 
                   Some begin try
                      (* Try just [%imp] first *)
                      resolve policy env loc e.exp_type
                     with
                     | _ ->
                       (* If above failed, try Some [%imp] *)
                       Forge.Exp.some & resolve policy env loc ty
                   end,
                   Optional)
              end
          end
      | _ -> a
      end
  | _ -> a

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let create_function_id = 
    let x = ref 0 in
    fun () -> incr x; "__imp__function__" ^ string_of_int !x

  let is_function_id = String.is_prefix "__imp__function__"

  let enter_expression e = match e.exp_desc with
    | Texp_apply (f, args) ->
        { e with
          exp_desc= Texp_apply (f, map (resolve_arg f.exp_loc e.exp_env) args) }

    | Texp_function (l, _::_::_, _) when l <> "" ->
        (* Eeek, label with multiple cases? *)
        warn (fun () ->
          eprintf "%a: Unexpected label with multiple function cases"
            Location.print_loc e.exp_loc);
        e
           
    | Texp_function (l, [case], e') when is_constraint_label l <> None ->
        (* If a pattern has a form l:x where [is_constraint_label l],
           then the value can be used as an instance of the same type.

           Here, the problem is that [leave_expression] does not take the same expression
           as here. Therefore we need small imperative trick. Attributes should be kept as they are...
        *)
        let fid = create_function_id () in
        let id = Ident.create fid in
        derived_candidates := (fid, (Longident.Lident fid, Path.Pident id, 
                                     { val_type = case.c_lhs.pat_type;
                                       val_kind = Val_reg;
                                       val_loc = case.c_lhs.pat_loc; (* CR jfuruse: make it ghost *)
                                       val_attributes = [] },
                                     false)) :: !derived_candidates;
        let case = { case with c_lhs = Forge.Pat.desc (Tpat_alias (case.c_lhs, id, {txt=fid; loc=case.c_lhs.pat_loc})) } in (* CR jfuruse: make the loc ghost *)
        { e with exp_desc = Texp_function (l, [case], e');
                 exp_attributes = ({txt=fid; loc=e.exp_loc}, PStr []) :: e.exp_attributes }

    | _ -> match is_imp e with
    | None -> e

    | Some policy -> resolve_imp policy e.exp_env e.exp_loc e.exp_type

  let leave_expression e = 
    let ids, others = partition (function ({Location.txt}, Parsetree.PStr[]) -> is_function_id txt | _ -> false) e.exp_attributes in
    match ids with
    | [] -> e
    | _::_::_ -> assert false 
    | [({txt},_)] -> 
        derived_candidates := List.filter (fun (fid, _) -> fid <> txt) !derived_candidates;
        { e with exp_attributes = others }
end

module Map = TypedtreeMap.MakeMap(MapArg)
