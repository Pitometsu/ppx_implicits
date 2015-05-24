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

(*
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
*)
  
let is_constraint_label l =
  let len = String.length l in
  if len >= 2 && String.unsafe_get l 0 = '_' then Some `Normal
  else if len >= 3 && String.sub l 0 2 = "?_" then Some `Optional
  else None

let rec extract_constraint_labels env ty = 
  let ty = Ctype.expand_head env ty in
  match repr_desc ty with
  | Tarrow(l, ty1, ty2, _) when is_constraint_label l <> None ->
      let cs, ty = extract_constraint_labels env ty2 in
      (l,ty1)::cs, ty
  | _ -> [], ty

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

let imp_type_policy env ty =
  match expand_repr_desc env ty with
  | Tconstr (p, _, _) -> 
      begin match p with
      | Pident _ ->
          (* __imp_policy__ must exit *)
          let p, td = Env.lookup_type (Lident "__imp_policy__") env in
          begin match p with
          | Pident _ -> Policy.from_type_decl td
          | _ -> (* found but defined in another module and opened *)
              None
          end
      | Pdot (mp, _, _) -> 
          (* mp.__imp_policy__ must exist *)
          Some (Policy.from_module_path env mp)
      | _ -> None
      end
  | _ -> None

    
let resolve_imp policy env loc ty =
  (* fix the policy if ty = __imp__ *)
  let policy = match policy with
    | Policy.Type ->
        begin match imp_type_policy env ty with
        | None -> assert false (* must handle error *)
        | Some p -> p
        end
    | _ -> policy
  in
  resolve policy env loc ty

(* ?l:None  where (None : X...Y.__imp__ option) has a special rule *) 
let resolve_arg loc env = function
  (* (l, None, Optional) means not applied *)
  | (l, Some e, Optional as a) when is_constraint_label l = Some `Optional ->
      begin match e.exp_desc with
      | Texp_construct ({Location.txt=Lident "None"}, _, []) ->
          begin match is_option_type env e.exp_type with
          | None -> assert false
          | Some ty ->
              begin match imp_type_policy env ty with
              | None -> a (* Think abount derived! *)
              | Some policy ->
                  (l, Some begin
                    try
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
  | a -> a

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

(*
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
*)
    
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
