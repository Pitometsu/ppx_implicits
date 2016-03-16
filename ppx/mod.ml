open Ppxx.Utils
open Utils.List
open Ppxx.Compilerlib
open Typpx.Compilerlib

open Asttypes
open Typedtree
open Types

open Format

module Forge = Typpx.Forge
  
(* CR jfuruse: bad state... *)
(* CR jfuruse: confusing with deriving spec *)                      
let derived_candidates = ref []

(* CR jfuruse: this is very slow, since it computes everything each time. *)
let get_candidates env loc spec ty =
  let f = Spec.candidates env loc spec in
  Candidate.uniq & f ty @ map snd !derived_candidates


module Runtime = struct
  (** Ppx_implicits.Runtime.t *)
  let is_imp_t_path = function
    | Path.Pdot(Pident{Ident.name="Ppx_implicits"},"t",_) -> true
    | _ -> false
      
  (** [make_embed e] builds [Ppx_implicits.Runtime.embed <e>] *)
  let embed e =
    Forge.Exp.(app (untyped [%expr Ppx_implicits.embed]) ["", e])
  
  (** [make_get e] builds [Ppx_implicits.get <e>] *)
  let get e =
    Forge.Exp.(app (untyped [%expr Ppx_implicits.get]) ["", e])
  
  (** [make_get e] builds [Ppx_implicits.get <e>] *)
  let from_Some e =
    Forge.Exp.(app (untyped [%expr Ppx_implicits.from_Some]) ["", e])
end

let is_imp_arg_type env ty = match expand_repr_desc env ty with
  | Tconstr (p, [ty; spec], _) when Runtime.is_imp_t_path p -> Some (ty, spec)
  | _ -> None

(* CR jfuruse: is_imp_arg and imp_type_spec can be shared *)
let is_imp_arg env l ty = 
  if not & Btype.is_optional l then None
  else match is_option_type env ty with
  | None -> None
  | Some ty -> is_imp_arg_type env ty

(* CR jfuruse: Now a bad name *)
let imp_type_spec env loc l ty (* option if [is_optional l] *) =
  let f ty = match is_imp_arg_type env ty with
    | Some (ty, spec) ->
        let spec = Specconv.from_type_expr env loc spec in
        (l, ty, Some spec, Runtime.embed, Runtime.get)
    | None -> 
        (l, ty, None, (fun x -> x), (fun x -> x))
  in
  if not & Btype.is_optional l then f ty
  else begin
    match is_option_type env ty with
    | None -> assert false
    | Some ty -> 
        let l, ty, spec_opt, conv, unconv = f ty in
        (l, ty, spec_opt,
         (fun e -> Forge.Exp.some env (conv e)),
         (fun e -> unconv (Runtime.from_Some e)))
  end 

module Klabel2 = struct
  (* Constraint labels must precede the other arguments *)
  let rec extract env ty = 
    let ty = Ctype.expand_head env ty in
    match repr_desc ty with
    | Tarrow(l, ty1, ty2, _) when (* Klabel.is_klabel l <> None || *) is_imp_arg env l ty1 <> None ->
        let cs, ty = extract env ty2 in
        (l,ty1)::cs, ty
    | _ -> [], ty
  
  let rec extract_aggressively env ty =
    let ty = Ctype.expand_head env ty in
    match repr_desc ty with
    | Tarrow(l, ty1, ty2, _) when gen_vars ty1 <> [] ->
        ([], ty)
        :: map
          (fun (cs, ty) -> (l,ty1)::cs, ty)
          (extract_aggressively env ty2)
    | _ -> [[], ty]
  end

(* Fix the candidates by adding type dependent part *)
let extract_candidate spec env loc { Candidate.aggressive; type_ } : ((label * type_expr * Spec.t * (expression -> expression)) list * type_expr) list =
  let f =
    if not aggressive then (fun type_ -> [Klabel2.extract env type_])
    else Klabel2.extract_aggressively env
  in
  flip map (f type_) & fun (args, ty) ->
    flip map args (fun (l,ty) ->
      let (l,ty,specopt,conv,_unconv) = imp_type_spec env loc l ty in
      (l,ty, (match specopt with Some x -> x | None -> spec), conv)),
    ty

(*
let extract_candidate spec env loc c = 
  let xs = extract_candidate spec env loc c in
  !!% "Cand: %a@." Candidate.format c;
  !!% "  => @[<v>%a@]@."
    (List.format "@," & fun ppf (subs,ty) ->
      List.format " " (fun ppf (l,ty,_spec,_conv) ->
        Format.fprintf ppf "(%s: %a)"
          l Printtyp.type_scheme ty) ppf subs;
      Format.fprintf ppf " => %a" Printtyp.type_scheme ty) xs;
  xs
*)

module Resolve_result = struct
  type t =
    | Ok of expression list list
    | MayLoop of expression list (** resolution aborted because of possible infinite loops *)

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
(** Used instance history. This is used to check the same instance is
    not used with types with not strictly decreasing size. *)

let rec resolve loc env : (trace * type_expr * Spec.t) list -> Resolve_result.t = function
  | [] -> Resolve_result.Ok [[]] (* one solution with the empty expression set *)
  | (trace,ty,spec)::problems ->
      let cs = get_candidates env loc spec ty in
      if !Options.debug_unif then begin
        !!% "Candidates:@.";
        iter (!!% "  %a@." Candidate.format) cs
      end;
      let cs = concat_map (fun c ->
        map (fun x -> c.Candidate.path, c.expr, x) & extract_candidate spec env loc c
      ) cs
      in
      Resolve_result.concat
      & flip map cs
      & resolve_cand loc env trace ty problems

(* CR jfuruse: loc is fixed argument *)
and resolve_cand loc env trace ty problems (path, expr, (cs,vty)) =

  let org_tysize = Tysize.size ty in 

  match assoc_opt path trace with
  | Some ty' when not & Tysize.(lt org_tysize (size ty')) ->
      (* recursive call of path and the type size is not strictly decreasing *)
      if !Options.debug_unif then begin
        !!% "  Checking %a <> ... using %a ... oops@."
          Printtyp.type_expr ty
          Path.format path;
        !!% "    @[<2>Non decreasing %%imp recursive dependency:@ @[<2>%a@ : %a (%s)@ =>  %a (%s)@]@]@." 
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
        match Ctype.instance_list env (vty::map (fun (_,ty,_spec,_conv) -> ty) cs) with
        | [] -> assert false
        | ivty :: ictys ->
            ivty, map2 (fun (l,_,spec,conv) icty -> (l,icty,spec,conv)) cs ictys
      in

      with_snapshot & fun () ->
        if !Options.debug_unif then begin
          !!% "  Checking %a <> %a, using %a ...@."
            Printtyp.type_expr ity
            Printtyp.type_expr ivty
            Path.format path;
        end;
        match protect & fun () -> Ctype.unify env ity ivty with
        | `Error (Ctype.Unify utrace) ->
            if !Options.debug_unif then begin
              !!% "    no@.";
              !!% "      Reason: @[%a@]@."
                (fun ppf utrace -> Printtyp.report_unification_error ppf
                  env utrace
                  (fun ppf -> fprintf ppf "Hmmm ")
                  (fun ppf -> fprintf ppf "with"))
                utrace;

              !!% "    Type 1: @[%a@]@." Printtyp.raw_type_expr  ity;
              !!% "    Type 2: @[%a@]@." Printtyp.raw_type_expr  ivty

            end;
            Resolve_result.Ok [] (* no solution *)
                   
        | `Error e -> raise e (* unexpected *)

        | `Ok _ ->
            if !Options.debug_unif then
              !!% "    ok: %a@." Printtyp.type_expr ity;
            
            let new_tysize = Tysize.size ty in

            if Tysize.(has_var new_tysize
                       && has_var org_tysize
                       && not & lt new_tysize org_tysize)
            then begin
              if !Options.debug_unif then begin
                !!% "    Tysize vars not strictly decreasing %s => %s@."
                  (Tysize.to_string org_tysize)
                  (Tysize.to_string new_tysize)
              end;
                 (* CR jfuruse: this is reported ambiguousity *) 
              Resolve_result.MayLoop [expr]
            end else

              (* Add the sub-problems *)
              let problems = map (fun (_,ty,spec,_conv) -> (trace',ty,spec)) cs @ problems in

              if !Options.debug_unif then
                !!% "    subproblems: @[<v>%a@]@."
                  (List.format "@," (fun ppf (_,ty,spec,_) ->
                    Format.fprintf ppf "%a / %s"
                      Printtyp.type_scheme ty
                      (Spec.to_string spec))) cs;
              
              match resolve loc env problems with
              | MayLoop es -> MayLoop es
              | Ok res_list ->
                  let build res =
                    let args, res = split_at (length cs) res in
                    Forge.Exp.(app expr (map2 (fun (l,_,_,conv) a -> (l,conv a)) cs args)) :: res
                  in
                  Ok (map build res_list)

let resolve env loc spec ty = with_snapshot & fun () ->

  if !Options.debug_resolve then !!% "@.RESOLVE: %a@." Location.format loc;

  close_gen_vars ty;

  if !Options.debug_resolve then !!% "  The type is: %a@." Printtyp.type_scheme ty;

  (* CR jfuruse: Only one value at a time so far *)
  match resolve loc env [([],ty,spec)] with
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
      
(* ?l:None  where (None : (ty,spec) Ppx_implicit.t option) has a special rule *) 
let resolve_arg loc env a = match a with
  (* (l, None, Optional) means curried *)
  | (l, Some e, Optional) when is_imp_arg env l e.exp_type <> None ->
      begin match Utils.is_none e with
      | None -> a (* explicitly applied *)
      | Some _ ->
          let ty = e.exp_type in (* _ option *)
          let (_l, ty, specopt, conv, _unconv) = imp_type_spec env loc l ty in
          begin match specopt with
          | None -> assert false (* CR jfuruse: error handling *)
          | Some spec ->
              (l, Some (conv (resolve env loc spec ty)), Optional)
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

    | Texp_function (l, [case], e') when is_imp_arg case.c_lhs.pat_env l case.c_lhs.pat_type <> None ->
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
        let ty = case.c_lhs.pat_type in
        let (_l, type_, _specopt, _conv, unconv) = imp_type_spec env loc l ty in
        let expr = unconv (Typpx.Forge.Exp.(ident path)) in
        
        derived_candidates := (fid, { Candidate.path; (* <- not actually path. see expr field *)
                                      expr;
                                      type_;
                                      aggressive = false } ) 
                              :: !derived_candidates;
        let case = { case with
                     c_lhs = Forge.(with_loc case.c_lhs.pat_loc & fun () -> Pat.desc (Tpat_alias (case.c_lhs, id, {txt=fid; loc= Ppxx.Helper.ghost case.c_lhs.pat_loc})))} 
        in
        Forge.Exp.mark fid { e with exp_desc = Texp_function (l, [case], e') }

    | Texp_function (l, [case], _e') when Btype.is_optional l -> 
        !!% "WHAT? %a : %s : %a@." Location.format e.exp_loc l Printtyp.type_scheme case.c_lhs.pat_type;
        e

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
    if !Options.debug_resolve then !!% "Unshadow...@.";
    Unshadow.Alias.insert str
end
