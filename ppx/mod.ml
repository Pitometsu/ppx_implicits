open Ppxx.Utils
open Ppxx.Compilerlib
open Typedtree
open Types
open Asttypes
open Longident (* has flatten *)
open List (* has flatten *)
open Format

module Forge = Typpx.Forge
  
(** Check [e [@imp ...]] for example, [assert false [@imp ...]] *)
let has_imp e = 
  let imps = flip map e.exp_attributes & function 
    | {txt="imp"}, payload -> Some (Spec.from_payload e.exp_env payload)
    | _ -> None
  in
  match flip filter imps & function Some _ -> true | None -> false with
  | [] -> None
  | [Some (`Ok x)] -> Some x
  | [Some (`Error err)] -> Spec.error e.exp_loc err
  | _ -> errorf "@[<2>%a:@ expression has multiple @@imp@]" Location.format e.exp_loc
  
type trace = (Path.t * type_expr) list
    
let rec resolve env get_cands : (trace * type_expr) list -> expression list list = function
  | [] -> [[]]
  | (trace,ty)::tr_tys ->
      let cands =
        let cs = get_cands ty in
        flip concat_map cs & fun { Candidate.path; expr; type_; aggressive } ->
          if not aggressive then [(path, expr, Klabel.extract env type_)]
          else map (fun cs_ty -> (path, expr, cs_ty)) & Klabel.extract_aggressively env type_
      in
      flip concat_map cands & fun (path, expr, (cs,vty)) ->
        match assoc_opt path trace with
        | Some ty' when not & Tysize.(lt (size ty) (size ty')) ->
            (* recursive call of path and the type size is not strictly decreasing *)
            if !Options.debug_unif then begin
              eprintf "  Checking %a <> ... using %a ... oops"
                Printtyp.type_expr ty
                Path.format path;
              eprintf "    @[<2>Non decreasing %%imp recursive dependency:@ @[<2>%a@ : %a (%s)@ =>  %a (%s)@]@]@." 
                Path.format path
                Printtyp.type_expr ty'
                (Tysize.(to_string & size ty'))
                Printtyp.type_expr ty
                (Tysize.(to_string & size ty));
            end;

            []

        | _ ->
            (* CR jfuruse: Older binding of path is no longer useful. Replace instead of add? *)
            let trace' = (path, ty) :: trace in 

            let ity = Ctype.instance env ty in
        
            let ivty, cs =
              match Ctype.instance_list env (vty::map snd cs) with
              | [] -> assert false
              | ivty :: ictys ->
                  ivty, map2 (fun (l,_) icty -> (l,icty)) cs ictys
            in

            with_snapshot & fun () ->
              if !Options.debug_unif then begin
                eprintf "  Checking %a <> %a, using %a ..."
                  Printtyp.type_expr ity
                  Printtyp.type_expr ivty
                  Path.format path;
              end;
              match protect & fun () -> Ctype.unify env ity ivty with
              | `Error (Ctype.Unify utrace) ->
                  if !Options.debug_unif then begin
                    eprintf "    no@.";
                    eprintf "      Reason: @[%a@]@."
                      (fun ppf utrace -> Printtyp.report_unification_error ppf
                        env utrace
                        (fun ppf -> fprintf ppf "Hmmm ")
                        (fun ppf -> fprintf ppf "with"))
                      utrace;
(*
                    eprintf "    Type 1: @[%a@]@." Printtyp.raw_type_expr  ity;
                    eprintf "    Type 2: @[%a@]@." Printtyp.raw_type_expr  ivty
*)
                  end;
                  [] (* no solution *)
                      
              | `Error e ->
                  (* CR jfuruse: this is unexpected therefore should fail ppx *)
                  eprintf "  Ctype.unify raised strange exception %s@." (Printexc.to_string e);
                  [] (* no solution *)
                      
              | `Ok _ ->
                  if !Options.debug_unif then
                    eprintf "    ok: %a@." Printtyp.type_expr ity;
                  
                  (* Add the sub-problems *)
                  let tr_tys = map (fun (_,ty) -> (trace',ty)) cs @ tr_tys in
                  flip map (resolve env get_cands tr_tys) & fun res ->
                    let rec app res cs = match res, cs with
                      | res, [] -> res, [] (* all the sub solutions are applied *)
                      | r::res, (l,_)::cs ->
                          let res, args = app res cs in
                          res, (l,r)::args
                      | _ -> assert false
                    in
                    let res, args = app res cs in
                    Forge.Exp.(app expr args) :: res

(* CR jfuruse: bad state... *)
(* CR jfuruse: confusing with deriving spec *)                      
let derived_candidates = ref []

let resolve env loc spec ty = with_snapshot & fun () ->

  if !Options.debug_resolve then eprintf "@.RESOLVE: %a@." Location.format loc;

  close_gen_vars ty;

  let get_cands =
    let f = Spec.candidates env loc spec in
    fun ty -> Candidate.uniq & f ty @ map snd !derived_candidates
  in

  (* CR jfuruse: Only one value at a time so far *)
  match resolve env get_cands [([],ty)] with
  | [[e]] -> Unshadow.Replace.replace e
  | [] ->
      errorf "@[<2>%a:@ no instance found for@ @[%a@]@]"
        Location.format loc
        Printtyp.type_expr ty
  | [es] ->
      errorf "@[<2>%a: @[<2>overloaded type has a too ambiguous type:@ @[%a@]@]@.@[<2>Following possible resolutions:@ @[<v>%a@]@]"
        Location.format loc
        Printtyp.type_expr ty
        (List.format "@," (Printast.expression 0)) (map Typpx.Untypeast.untype_expression es)
  | _ -> assert false (* we only resolve one instance at a time *)

(* get the spec for [%imp] from its type *)
let imp_type_spec env loc ty =
  match expand_repr_desc env ty with
  | Tconstr (p, _, _) -> 
      begin match p with
      | Pident _ ->
          (* Oh it's local... *)
          (* __imp_spec__ must exit *)
          let p, td = 
            try
              let p, td = Env.lookup_type (Lident "__imp_spec__") env in
              match p with
              | Pident id when Ident.persistent id -> p, td
              | _ -> raise Exit (* __imp_spec__ exists but in some module, not in the top *)
            with
            | Not_found | Exit ->
                errorf "%a: Current module has no implicit spec declaration [%%%%imp_spec SPEC]"
                  Location.format loc
          in
          `Ok (Spec.from_type_decl env td.type_loc p td)
      | Pdot (mp, _, _) -> 
          (* <mp>.__imp_spec__ must exist *)
          begin match Spec.from_module_path env loc mp with
          | `Ok x -> `Ok x
          | `Error (`No_imp_spec (mp_loc, mp)) ->
              errorf "@[<2>%a: [%%imp] expression has type %a,@ but module %a has no declaration [%%%%imp_spec SPEC].@ %a: module %a is defined here.@]"
                Location.format loc
                Printtyp.type_expr ty (* reset? *)
                Path.format mp
                Location.format mp_loc
                Path.format mp
          end
      | _ -> assert false (* impos: F(X) *)
      end
  | _ -> `Error `Strange_type

(* Eval type spec *)      
let get_spec spec env loc ty = match spec with
  | Spec.Type ->
      (* fix the spec for [%imp] *)
      begin match imp_type_spec env loc ty with
      | `Error `Strange_type ->
          errorf "%a: [%%%%imp] has a bad type: %a" 
            Location.format loc
            Printtyp.type_expr ty
      | `Ok p -> p
      end
  | _ -> spec
    

let resolve_imp env loc spec ty =
  let spec = get_spec spec env loc ty in
  resolve env loc spec ty

let is_none e = match e.exp_desc with
  | Texp_construct ({Location.txt=Lident "None"}, _, []) -> 
      begin match is_option_type e.exp_env e.exp_type with
      | None -> assert false (* CR jfuruse: input is type-corrupted... *)
      | Some ty -> Some ty
      end
  | _ -> None
    
(* ?_l:None  where (None : X...Y.name option) has a special rule *) 
let resolve_arg loc env a = match a with
  (* (l, None, Optional) means not applied *)
  | (l, Some e, Optional) when Klabel.is_klabel l = Some `Optional ->
      begin match is_none e with
      | None -> a (* explicitly applied *)
      | Some ty ->
          begin match imp_type_spec env loc ty with
          | `Error `Strange_type -> a (* Think about derived! *)
          | `Ok spec ->
              (l, 
               Some begin try
                  (* CR jfuruse: we can have ambiguous resolution
                     for t option and t *)
                  (* Try just [%imp] first *)
                  resolve env loc spec e.exp_type
                 with
                 | _ ->
                     (* If above failed, try Some [%imp] *)
                     Forge.Exp.some env & resolve env loc spec ty
               end,
               Optional)
          end
      end
  | _ -> a

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let create_function_id = 
    let x = ref 0 in
    fun () -> incr x; "__imp__function__" ^ string_of_int !x

  let is_function_id = String.is_prefix "__imp__function__"

  module Ppxxx = struct
    (* CR jfuruse: should be moved to Ppxx *)

    let mark_expression txt e =
      { e with
        exp_attributes = ({txt; loc= Ppxx.Helper.ghost e.exp_loc}, Parsetree.PStr []) :: e.exp_attributes }
        
    let partition_expression_marks e f =
      let g = function
        | {txt}, Parsetree.PStr [] when f txt -> `Left txt
        | a -> `Right a
      in
      let marks, exp_attributes = partition_map g e.exp_attributes in
      marks,
      { e with exp_attributes }
  end
    
  let enter_expression e = match e.exp_desc with
    | Texp_apply (f, args) ->
        (* resolve omitted ?_x arguments *)
        { e with
          exp_desc= Texp_apply (f, map (resolve_arg f.exp_loc e.exp_env) args) }

    | Texp_function (l, _::_::_, _) when l <> "" ->
        (* Eeek, label with multiple cases? *)
        warnf "%a: Unexpected label with multiple function cases"
          Location.format e.exp_loc;
        e
           
    | Texp_function (l, [case], e') when Klabel.is_klabel l <> None ->
        (* If a pattern has a form l:x where [Klabel.is_klabel l],
           then the value can be used as an instance of the same type.

           Here, the problem is that [leave_expression] does not take the same expression
           as here. Therefore we need small imperative trick. Attributes should be kept as they are...
        *)
        let fid = create_function_id () in
        let lid = Longident.Lident fid in
        let id = Ident.create fid in
        let path = Path.Pident id in
        derived_candidates := (fid, { Candidate.lid;
                                      path;
                                      expr = Typpx.Forge.Exp.(ident path); 
                                      type_ = case.c_lhs.pat_type;
                                      aggressive = false } ) 
                              :: !derived_candidates;
        let case = { case with
                     c_lhs = Forge.(with_loc case.c_lhs.pat_loc & fun () -> Pat.desc (Tpat_alias (case.c_lhs, id, {txt=fid; loc= Ppxx.Helper.ghost case.c_lhs.pat_loc})))} 
        in
        Ppxxx.mark_expression fid { e with exp_desc = Texp_function (l, [case], e') }

    | _ ->
        match has_imp e with
        | None -> e
        | Some spec -> resolve_imp e.exp_env e.exp_loc spec e.exp_type

  let leave_expression e =
    match Ppxxx.partition_expression_marks e & fun txt -> is_function_id txt with
    | [], e -> e
    | [txt], e ->
        derived_candidates := filter (fun (fid, _) -> fid <> txt) !derived_candidates;
        e
    | _ -> assert false
end

module Map = struct
  include TypedtreeMap.MakeMap(MapArg)

  let map_structure str =
    (* This is tricky. Unshadow.reset () is placed here, knowing that
       map_structure is the entry point for structure in TyPPX *)
    Unshadow.reset ();
    let str = map_structure str in
    if !Options.debug_resolve then eprintf "Unshadow...@.";
    Unshadow.Alias.insert str
end
