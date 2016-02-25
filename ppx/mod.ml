open Ppxx.Utils
open Ppxx.Compilerlib
open Typedtree
open Types
open Asttypes
open Longident (* has flatten *)
open Utils.List (* has flatten *)
open Format

module Forge = Typpx.Forge
  
(** Ppx_implicits.Runtime.t *)
let is_imp_type_path = function
  | Path.Pdot(Pdot(Pident{Ident.name="Ppx_implicits"},"Runtime",_),"t",_) -> true
  | _ -> false
    
(* get the spec for [%imp] from its type *)
let imp_type_spec env loc ty0 =
  match expand_repr_desc env ty0 with
  | Tconstr (p, [ty; spec], _) when is_imp_type_path p ->
      let spec = Specconv.from_type_expr env loc spec in
      Some (ty, spec)
(* We must use the following error handling by resultize from_type_expr 
          errorf "%a: expression has type %a,@ where type %a does not encode implicit resolution spec."
            Location.format loc
            Printtyp.type_expr ty0 (* reset? *)
            Printtyp.type_expr spec
*)
  | _ -> None

(* Fix the candidates by adding type dependent part *)
let extract_candidate env get_cands { Candidate.aggressive; type_ } =
  let f =
    if not aggressive then (fun type_ -> [Klabel.extract env type_])
    else Klabel.extract_aggressively env
  in
  flip map (f type_) & fun (ltys, ty) ->
    map (fun (l,ty) -> (l,ty,get_cands)) ltys, ty

module Resolve_result = struct
  type t =
    | Ok of expression list list
    | MayLoop of expression list (* resolution aborted because of possible infinite loops *)

  let concat xs =
    match
      flip partition_map xs & function
        | Ok ys -> `Right ys
        | MayLoop ys -> `Left ys
    with
    | [], oks -> Ok (concat oks)
    | mayloops, _ -> MayLoop (concat mayloops)
end
  
type trace = (Path.t * type_expr) list

let rec resolve env : (trace * type_expr * (type_expr -> Candidate.t list)) list -> Resolve_result.t = function
  | [] -> Resolve_result.Ok [[]] (* one solution with the empty expression set *)
  | (trace,ty,get_cands)::tr_ty_gcs ->
      let cs = get_cands ty in
      if !Options.debug_unif then begin
        eprintf "Candidates:@.";
        iter (eprintf "  %a@." Candidate.format) cs
      end;
      let cands = concat_map (fun c ->
        map (fun x -> c.Candidate.path, c.expr, x) & extract_candidate env get_cands c
      ) (get_cands ty)
      in
      Resolve_result.concat
      & flip map cands
      & resolve_cand env trace ty tr_ty_gcs

and resolve_cand env trace ty tr_ty_gcs (path, expr, (cs,vty)) =

  let org_tysize = Tysize.size ty in 

  match assoc_opt path trace with
  | Some ty' when not & Tysize.(lt org_tysize (size ty')) ->
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
      
      Resolve_result.Ok []

  | _ ->
      (* CR jfuruse: Older binding of path is no longer useful. Replace instead of add? *)
      let trace' = (path, ty) :: trace in 

      let ity = Ctype.instance env ty in
     
      let ivty, cs =
        match Ctype.instance_list env (vty::map (fun (_,x,_) -> x) cs) with
        | [] -> assert false
        | ivty :: ictys ->
            ivty, map2 (fun (l,_,gc) icty -> (l,icty,gc)) cs ictys
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

              eprintf "    Type 1: @[%a@]@." Printtyp.raw_type_expr  ity;
              eprintf "    Type 2: @[%a@]@." Printtyp.raw_type_expr  ivty

            end;
            Resolve_result.Ok [] (* no solution *)
                   
        | `Error e -> raise e (* unexpected *)

        | `Ok _ ->
            if !Options.debug_unif then
              eprintf "    ok: %a@." Printtyp.type_expr ity;
            
            let new_tysize = Tysize.size ty in

            if Tysize.(has_var new_tysize
                       && has_var org_tysize
                       && not & lt new_tysize org_tysize)
            then begin
              begin eprintf "    Tysize vars not strictly decreasing %s => %s@."
                  (Tysize.to_string org_tysize)
                  (Tysize.to_string new_tysize)
              end;
                 (* CR jfuruse: this is reported ambiguousity *) 
              Resolve_result.MayLoop [expr]
            end else

              (* Add the sub-problems *)
              let tr_ty_gcs = map (fun (_,ty,gc) -> (trace',ty,gc)) cs @ tr_ty_gcs in
              match resolve env tr_ty_gcs with
              | MayLoop es -> MayLoop es
              | Ok res_list ->
                  let build res =
                    let args, res = split_at (length cs) res in
                    Forge.Exp.(app expr (map2 (fun (l,_,_) a -> (l,a)) cs args)) :: res
                  in
                  Ok (map build res_list)

(* CR jfuruse: bad state... *)
(* CR jfuruse: confusing with deriving spec *)                      
let derived_candidates = ref []

let resolve env loc spec ty = with_snapshot & fun () ->

  if !Options.debug_resolve then eprintf "@.RESOLVE: %a@." Location.format loc;

  close_gen_vars ty;

  if !Options.debug_resolve then eprintf "The type is: %a@." Printtyp.type_scheme ty;

  let get_cands =
    let f = Spec.candidates env loc spec in
    fun ty -> Candidate.uniq & f ty @ map snd !derived_candidates
  in

  (* CR jfuruse: Only one value at a time so far *)
  match resolve env [([],ty,get_cands)] with
  | MayLoop es -> 
      errorf "%a:@ The experssion has type @[%a@] which is too ambiguous to resolve this implicit.@ @[<2>The following instances may cause infinite loop of the resolution:@ @[<2>%a@]@]"
        Location.format loc
        Printtyp.type_expr ty
        (* CR jfuruse: should define a function for printing Typedtree.expression *)
        (List.format ",@," Utils.format_expression) es
  | Ok [] ->
      errorf "%a:@ no instance found for@ @[%a@]"
        Location.format loc
        Printtyp.type_expr ty
  | Ok (_::_::_ as es) ->
      let es = map (function [e] -> e | _ -> assert false) es in
      errorf "%a: overloaded type has a too ambiguous type:@ @[%a@]@ @[<2>Following possible resolutions:@ @[<v>%a@]"
        Location.format loc
        Printtyp.type_expr ty
        (List.format "@," Utils.format_expression) es
  | Ok [es] ->
      match es with
      | [e] -> Unshadow.Replace.replace e
      | _ -> assert false
      
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
          | None -> a (* Think about derived! *)
          | Some (ty, spec) ->
              (l, 
               Some begin
                 (* Ppx_implicits.Runtime.embed e *)
                 (* CR jfuruse: TODO: Ppx_implicits.Runtime.embed (Ppx_implicits.Runtime.from_Some e) => e *)
                 Forge.Exp.(app (untyped [%expr Ppx_implicits.Runtime.embed])
                              ["", resolve env loc spec ty])
               end,
               Optional)
          end
      end
  | _ -> a

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  (* Code transformations independent each other must be embeded
     into one  AST mapper, and one part must be scattered into 
     more than two places. Very hard to read. *)

  let create_function_id = 
    let x = ref 0 in
    fun () -> incr x; "__imp__arg__" ^ string_of_int !x

  let is_function_id = String.is_prefix "__imp__arg__"

  let enter_expression e = match e.exp_desc with
    | Texp_apply (f, args) ->
        (* Resolve omitted ?_x arguments *)
        { e with
          exp_desc= Texp_apply (f, map (resolve_arg f.exp_loc e.exp_env) args) }

    | Texp_function (l, _::_::_, _) when l <> "" ->
        (* Eeek, label with multiple cases? *)
        warnf "%a: Unexpected label with multiple function cases"
          Location.format e.exp_loc;
        e

    | Texp_function (l, [case], e') when Klabel.is_klabel l <> None ->
        (* Handling derived implicits, part 1 of 2 *)
        (* If a pattern has a form l:x where [Klabel.is_klabel l],
           then the value can be used as an instance of the same type.

           Hack: the problem is that [leave_expression] does not take the same expression
           as here. Therefore we need small imperative trick. Attributes should be kept as they are...

           `(fun ?_x:(x as __imp_arg__0) -> ..)[@__imp_arg_0]`,
           adding an association of `"__imp_arg__0"` and the candidate of
           `__imp_arg__0` to `derived_candidates`.
        *)
        let fid = create_function_id () in
        let id = Ident.create fid in
        let path = Path.Pident id in
        let loc = case.c_lhs.pat_loc in
        let env = case.c_lhs.pat_env in
        let expr, type_embed = match Klabel.is_klabel l with
          | None -> assert false
          | Some `Normal -> Typpx.Forge.Exp.(ident path), case.c_lhs.pat_type
          | Some `Optional ->
              Typpx.Forge.Exp.(app (untyped [%expr Ppx_implicits.Runtime.from_Some]) ["", ident path]),
              Typecore.extract_option_type env case.c_lhs.pat_type
        in
        let expr, type_ = match imp_type_spec env loc type_embed with
          | Some (ty, _spec) ->
              Typpx.Forge.Exp.(app (untyped [%expr Ppx_implicits.Runtime.get]) ["", expr]),
              ty
          | None ->
              expr, type_embed
        in

        derived_candidates := (fid, { Candidate.path; (* <- not actually path. see expr field *)
                                      expr;
                                      type_;
                                      aggressive = false } ) 
                              :: !derived_candidates;
        let case = { case with
                     c_lhs = Forge.(with_loc case.c_lhs.pat_loc & fun () -> Pat.desc (Tpat_alias (case.c_lhs, id, {txt=fid; loc= Ppxx.Helper.ghost case.c_lhs.pat_loc})))} 
        in
        Forge.Exp.mark fid { e with exp_desc = Texp_function (l, [case], e') }

    | _ -> e

  let leave_expression e =
    (* Handling derived implicits, part 2 of 2 *)
    match Forge.Exp.partition_marks e & fun txt -> is_function_id txt with
    | [], e -> e
    | [txt], e ->
        (* Hack:
           
           Remove the association of `"__imp_arg__0"` and the candidate of
           `__imp_arg__0` from `derived_candidates`.
        *)
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
