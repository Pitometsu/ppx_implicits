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
  (** [Ppx_implicits.Runtime.t] *)
  let is_imp_t_path = function
    | Path.Pdot(Pident{Ident.name="Ppx_implicits"},"t",_) -> true
    | _ -> false
      
  (** [embed e] builds [Ppx_implicits.Runtime.embed <e>] *)
  let embed e =
    Forge.Exp.(app (untyped [%expr Ppx_implicits.embed]) [Nolabel, e])
  
  (** [get e] builds [Ppx_implicits.get <e>] *)
  let get e =
    Forge.Exp.(app (untyped [%expr Ppx_implicits.get]) [Nolabel, e])
  
  (** [from_Some e] builds [Ppx_implicits.from_Some <e>] *)
  let from_Some e =
    Forge.Exp.(app (untyped [%expr Ppx_implicits.from_Some]) [Nolabel, e])
end

(** Check it is [(<ty>, <spec>) Ppx_implicits.t] *)
let is_imp_arg_type env ty = match expand_repr_desc env ty with
  | Tconstr (p, [ty; spec], _) when Runtime.is_imp_t_path p -> Some (ty, spec)
  | _ -> None

let check_arg env loc l ty =
  let f ty = match is_imp_arg_type env ty with
    | Some (ty, spec) ->
        let spec = Specconv.from_type_expr env loc spec in
        (ty, Some spec, Runtime.embed, Runtime.get)
    | None -> 
        (ty, None, (fun x -> x), (fun x -> x))
  in
  if not & Btype.is_optional l then f ty
  else begin
    match is_option_type env ty with
    | None -> (* this is pretty strange situation *)
        (ty, None, (fun x -> x), (fun x -> x))
    | Some ty -> 
        let ty, spec_opt, conv, unconv = f ty in
        match spec_opt with
        | None -> (ty, None, (fun x -> x), (fun x -> x))
        | Some _ -> 
            (ty, spec_opt,
             (fun e -> Forge.Exp.some env (conv e)),
             (fun e -> unconv (Runtime.from_Some e)))
  end 

module Klabel2 = struct
  (* Constraint labels must precede the other arguments *)
  let rec extract env ty = 
    let ty = Ctype.expand_head env ty in
    match repr_desc ty with
    | Tarrow(l, ty1, ty2, _) ->
        begin match check_arg env Location.none l ty1 with
        | (_, Some _, _, _) ->
            let cs, ty = extract env ty2 in
            (l,ty1)::cs, ty
        | _ -> [], ty
        end
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
let extract_candidate spec env loc { Candidate.aggressive; type_ } : ((arg_label * type_expr * Spec.t * (expression -> expression)) list * type_expr) list =
  let f =
    if not aggressive then (fun type_ -> [Klabel2.extract env type_])
    else Klabel2.extract_aggressively env
  in
  flip map (f type_) & fun (args, ty) ->
    (flip map args (fun (l,ty) ->
      let (ty,specopt,conv,_unconv) = check_arg env loc l ty in
      ( l
      , ty
      , (match specopt with
         | Some x -> x
         | None -> spec (* inherit! *))
      , conv))
    , ty)

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

let rec resolve retmark loc env : (trace * type_expr * Spec.t) list -> Resolve_result.t = function
  | [] -> Resolve_result.Ok [[]] (* one solution with the empty expression set *)
  | (trace,ty,spec)::problems ->
      let cs = get_candidates env loc spec ty in
      if !Debug.debug_unif then begin
        !!% "Candidates:@.";
        iter (!!% "  %a@." Candidate.format) cs
      end;
      let r1 = retmark in
      let cs = concat_map (fun c ->
        map (fun x -> 
               let r = Random.int 99999 in
               ignore r;
               (* !!% " random %d %d %s@." r1 r (Path.name c.Candidate.path); *)
               (r1, r, c.Candidate.path, c.expr, x) ) & extract_candidate spec env loc c
      ) cs
      in
      Resolve_result.concat
      & flip map cs
      & resolve_cand loc env trace ty problems

(* CR jfuruse: Once given, loc is constant *)
and resolve_cand loc env trace ty problems (rand1, rand, path, expr, (cs,vty)) =
  (* !!% " resolve_cand %d %d %s@." rand1 rand (Path.name path); *)

  let org_tysize = Tysize.size ty in 

  let isundec path = Re_pcre.pmatch (Re_pcre.regexp "UNDECIDABLE") path in
  (* let isundec _ = false in *)

  match assoc_opt path trace with
  | Some ty' when ( not & Tysize.(lt org_tysize (size ty')) ) && ( not (isundec (Path.name path)) ) ->
      (* !!% "qqqq %s@." (Path.name path); *)
      (* recursive call of path and the type size is not strictly decreasing *)
      if !Debug.debug_unif then begin
        !!% "  Checking %a <> ... using %a ... oops@."
          Printtyp.type_expr ty
          Path.format path;
        !!% "    @[<2>Skip this candidate because of non decreasing %%imp recursive dependency:@ @[<2>%a@ : %a (%s)@ =>  %a (%s)@]@]@." 
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
        | [] -> assert false (* impos *)
        | ivty :: ictys ->
            ivty, map2 (fun (l,_,spec,conv) icty -> (l,icty,spec,conv)) cs ictys
      in

      with_snapshot & fun () ->
        if !Debug.debug_unif then begin
          !!% "  Checking %a <> %a, using %a ...@."
            Printtyp.type_expr ity
            Printtyp.type_expr ivty
            Path.format path;
        end;
(*
              !!% "    Type 1: @[%a@]@." Printtyp.raw_type_expr  ity;
              !!% "    Type 2: @[%a@]@." Printtyp.raw_type_expr  ivty;
              !!% "    Type 1: @[%a@]@." Printtyp.type_scheme  ity;
              !!% "    Type 2: @[%a@]@." Printtyp.type_scheme  ivty;
*)
        let check a b = match b.desc with
          | Tpackage (p,_,_) when Path.name p = "Algebra_Mult_ABC" -> true
          | _ -> false
        in
        let rec chekk lim a = match lim with
         | 0 -> false
         | _ -> (
          (* !!% "lile %d %d@." (List.length a) lim; *)
          if List.length a = 4
          then
            let b = List.nth a 2 in
            let c = List.nth a 3 in
            match (b,c) with
             | ((b,_),(c,d)) ->
               let b = Printtyp.tree_of_type_scheme b in
               let c = Printtyp.tree_of_type_scheme c in
               let d = Printtyp.tree_of_type_scheme d in
               match (b,c,d) with
                 | (Outcometree.Otyp_constr (Outcometree.Oide_ident b,_),Outcometree.Otyp_constr (Outcometree.Oide_dot (_,c),_),_) ->
(*
              !!% "    Type 1: @[%a@]@." Printtyp.type_scheme  ity;
              !!% "    Type 2: @[%a@]@." Printtyp.type_scheme  ivty;
*)
                      (* !!% "b = %s@." b; *)
                      String.length b > 5 && String.sub b 0 6 = "*uniq*" && c = "occupy"
(*
                 | (Outcometree.Otyp_constr (Outcometree.Oide_ident b,_),_,_) ->
                      String.length b > 5 && String.sub b 0 6 = "*uniq*"
*)
                 | _ -> false
             | _ -> false
          else
          if List.length a = 6
          then
            let b = List.nth a 0 in
            let c = List.nth a 1 in
            match (b,c) with
(*
             | ((b,_),(c,_)) ->
                   match (protect & fun () -> Ctype.unify env c b) with
                     | Error (Ctype.Unify utrace) -> chekk (lim-1) utrace
                     | Ok _ -> true
                     | _ -> false
*)
             | _ -> false
          else false
         )
        in
        let proxy a = match a with
          | Ok a -> Ok (Some a)
          | Error (Ctype.Unify utrace) when chekk 3 utrace -> Ok None
          (* | Error a when check ity ivty -> Error a *)
          | Error a -> Error a
        in
        let cnvv a = match a with
          | Outcometree.Otyp_abstract _ -> "wqwq"
          | Outcometree.Otyp_stuff _ -> "wqwq"
          | Outcometree.Otyp_poly _ -> "wqwq"
          | Outcometree.Otyp_constr (Outcometree.Oide_ident a,_) -> a
          | _ -> "dcds"
        in
        let wwww a = match a with
          (* | Ctype.Variant _ -> "Variant" *)
          (* | Ctype.Obj _ -> "Obj" *)
          (* | Ctype.Escape _ -> "Escape" *)
          (* | Ctype.Incompatible_fields _ -> "Incomfi" *)
          (* | Ctype.Rec_occur _ -> "Rec_occur" *)
          | (a,b) ->
             let c = Printtyp.tree_of_type_scheme a in
             ignore c;
(*
             !!% "    KU1: @[%a@]@." Printtyp.type_scheme  a;
             !!% "    KU2: @[%a@]@." Printtyp.type_scheme  b;
             !!% "    KU3: %s@." (cnvv c);
*)
             "Unk"
        in
        (* !!% " match %d %d %s@." rand1 rand (Path.name path); *)
        match proxy (protect & fun () -> Ctype.unify env ity ivty) with
        | Error (Ctype.Unify utrace) ->
            if !Debug.debug_unif then begin
              !!% "    no@.";
              !!% "      Reason: @[%a@]@."
                (fun ppf utrace -> Printtyp.report_unification_error ppf
                  env utrace
                  (fun ppf -> fprintf ppf "Unification error ")
                  (fun ppf -> fprintf ppf "with"))
                utrace;

              !!% " %d %d %s@." rand1 rand (Path.name path);
              !!% "    Type 1: @[%a@]@." Printtyp.raw_type_expr  ity;
              !!% "    Type 2: @[%a@]@." Printtyp.raw_type_expr  ivty;
              !!% "kuku%s@." (String.concat " " (List.map wwww utrace))

            end;
            Resolve_result.Ok [] (* no solution *)
                   
        | Error e -> raise e (* unexpected *)

        | Ok _ ->
(*
            !!% " match ok %d %d %s@." rand1 rand (Path.name path);
            !!% "    Type 1: @[%a@]@." Printtyp.type_scheme  ity;
            !!% "    Type 2: @[%a@]@." Printtyp.type_scheme  ivty;
*)
            if !Debug.debug_unif then
              !!% "    ok: %a@." Printtyp.type_expr ity;
            
            let new_tysize = Tysize.size ty in

            if false (* Tysize.(has_var new_tysize
                       && has_var org_tysize
                       && not & lt new_tysize org_tysize) *)
            then begin
              if !Debug.debug_unif then begin
                !!% "    Tysize vars not strictly decreasing %s => %s@."
                  (Tysize.to_string org_tysize)
                  (Tysize.to_string new_tysize)
              end;
                 (* CR jfuruse: this is reported ambiguousity *) 
              Resolve_result.MayLoop [expr]
            end else

              (* Add the sub-problems *)
              let problems = map (fun (_,ty,spec,_conv) -> (trace',ty,spec)) cs @ problems in

              if !Debug.debug_unif then
                !!% "    subproblems: [ @[<v>%a@] ]@."
                  (List.format "@," (fun ppf (_,ty,spec,_) ->
                    Format.fprintf ppf "%a / %s"
                      Printtyp.type_scheme ty
                      (Spec.to_string spec))) cs;
              
              (* !!% " next resolve %d %d %s@." rand1 rand (Path.name path); *)
              match resolve (rand1*100+1) loc env problems with
              | MayLoop es -> MayLoop es
              | Ok res_list ->
                  (* !!% "return to %d from %d@ after resolve problems." rand1 rand; *)
                  let build res =
                    let args, res = split_at (length cs) res in
                    Forge.Exp.(app expr (map2 (fun (l,_,_,conv) a -> (l,conv a)) cs args)) :: res
                  in
                  Ok (map build res_list)

module type Config = sig
  val exp_function_enabled : bool
  val final_check_enabled : bool
end

module MakeFunctions (C : Config) = struct

let resolve env loc spec ty = with_snapshot & fun () ->

  if !Debug.debug_resolve then !!% "@.RESOLVE: %a@." Location.format loc;

  close_gen_vars ty;

  if !Debug.debug_resolve then !!% "  The type is: %a@." Printtyp.type_scheme ty;

  let retmark = Random.int 99999 in
  (* CR jfuruse: Only one value at a time so far *)
  match resolve retmark loc env [([],ty,spec)] with
  | MayLoop es -> 
      (* !!% "last resolve MayLoop@."; *)
      raise_errorf "%a:@ The experssion has type @[%a@] which is too ambiguous to resolve this implicit.@ @[<2>The following instances may cause infinite loop of the resolution:@ @[<2>%a@]@]"
        Location.format loc
        Printtyp.type_expr ty
        (* CR jfuruse: should define a function for printing Typedtree.expression *)
        (List.format ",@," Utils.format_expression) es

  | Ok [] ->
     (* !!% "last resolve []@."; *)
     if C.final_check_enabled
     then
      raise_errorf "%a:@ KUKU no instance found for@ @[%a@]"
        Location.format loc
        Printtyp.type_expr ty
     else None

  | Ok (ea::eb::_ as es) ->
      (* !!% "last resolve _::_::_@."; *)
      let es = map (function [e] -> e | _ -> assert false (* impos *)) es in

      let convv a =
        let empty = Array.make 0 "" in
        match a with
        | Typedtree.Texp_apply (a,_) -> (match a.exp_desc with
              | Texp_ident (p,_,_) -> (* !!% "ident%s@." (Path.name p); *)
                                      try
                                        let res = (Re_pcre.extract (Re_pcre.regexp "OVER_(.*)_LAPPING") (Path.name p)) in
                                        (* !!% " OLAPI %s@." (Path.name p); *)
                                        res
                                      with Not_found -> empty
              | _ -> empty
           )
        | _ -> empty
      in

      let convv2 a =
        let empty = Array.make 0 "" in
        match a with
        | Typedtree.Texp_apply (a,_) -> (match a.exp_desc with
              | Texp_ident (p,_,_) -> 
                                      try
                                        let res = (Re_pcre.extract (Re_pcre.regexp "OVER_(.*)_LAPPABLE") (Path.name p)) in
                                        (* !!% " OLAPA %s@." (Path.name p); *)
                                        res
                                      with Not_found -> empty
              | _ -> empty
           )
        | _ -> empty
      in

      let tes =
         (* !!% "start %d %b@." (List.length es) (ea == eb); *)
         let to_remove = map (fun a -> convv (a.exp_desc)) es in
         let remover a b =
           let c = convv2 (a.exp_desc) in
           let d = List.for_all (fun a -> not (a=c)) to_remove in
           if not d then b else a :: b
         in
         List.fold_right remover es []
      in
      if List.length tes == 1
      then
        ( (* !!% "last resolve Length tes == 1@."; *)
        match tes with
        | [e] -> Some (Unshadow.Replace.replace e)
	| _ -> assert false (* impos *) )
	else
        if List.length tes == 0
        then 
           ( (* !!% "last resolve Length tes == 0@."; *)
           if C.final_check_enabled
           then
             raise_errorf "%a:@ KUKU2 no instance found for@ @[%a@]"
             Location.format loc
             Printtyp.type_expr ty
           else None )
        else
        if C.final_check_enabled
        then
          ( (* !!% "last resolve Length tes > 1@."; *)
          raise_errorf "%a: KUKU This implicit has too ambiguous type:@ @[%a@]@ @[<2>Following possible resolutions:@ @[<v>%a@]"
            Location.format loc
            Printtyp.type_expr ty
            (List.format "@," Utils.format_expression) tes )
        else None 
  | Ok [es] ->
      (* !!% "last resolve [es]@."; *)
      match es with
      | [e] -> Some (Unshadow.Replace.replace e)
      | _ -> assert false (* impos *)

      
(* ?l:None  where (None : (ty,spec) Ppx_implicit.t option) has a special rule *) 
let resolve_omitted_imp_arg loc env a =
  match a with
  (* (l, None, Optional) means curried *)
  | ((Optional _ as l), Some e) ->
      begin match Utils.is_none e with
      | None -> a (* explicitly applied *)
      | Some _ -> (* omitted *)
          let (ty, specopt, conv, _unconv) = check_arg env loc l e.exp_type in
          match specopt with
          | None -> a
          | Some spec ->
             let result = resolve env loc spec ty in
             match result with
             | None -> a
             | Some res -> (l, Some (conv res))
      end
  | _ -> a

end (* MakeFunctions *)

module MapArg (C : Config) : TypedtreeMap.MapArgument = struct
  module MF = MakeFunctions(C)
  open MF
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
          exp_desc= Texp_apply (f, map (resolve_omitted_imp_arg f.exp_loc e.exp_env) args) }

    | Texp_function { arg_label=l; param=_; cases= _::_::_; partial= _} when l <> Nolabel ->
        (* Eeek, label with multiple cases? *)
        warnf "%a: Unexpected label with multiple function cases"
          Location.format e.exp_loc;
        e

    | Texp_function { arg_label=l; param; cases= [case]; partial } when C.exp_function_enabled ->
        let p = case.c_lhs in
        begin match check_arg p.pat_env p.pat_loc l p.pat_type with
        | (_, None, _, _) -> e
        | (type_, Some _spec, _conv, unconv) -> (* CR jfuruse: specs are ignored *)
            let fid = create_function_id () in
            let id = Ident.create fid in
            let path = Path.Pident id in
            let expr = unconv (Typpx.Forge.Exp.(ident path)) in
            
            derived_candidates := (fid, { Candidate.path; (* <- not actually path. see expr field *)
                                          expr;
                                          type_;
                                          aggressive = false } ) 
                                  :: !derived_candidates;
            let case = { case with
              c_lhs = Forge.(with_loc p.pat_loc & fun () -> Pat.desc (Tpat_alias (p, id, {txt=fid; loc= Ppxx.Helper.ghost p.pat_loc})))} 
            in
            Forge.Exp.mark fid { e with exp_desc = Texp_function { arg_label=l; param; cases= [case]; partial } }
        end
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
    | _ -> assert false (* impos *)
end


module Map (C : Config) = struct
  module MA = MapArg (C)
  include TypedtreeMap.MakeMap(MA)

  let map_structure str =
    (* This is tricky. Unshadow.reset () is placed here, knowing that
       map_structure is the entry point for structure in TyPPX *)
    Unshadow.reset ();
    let str = map_structure str in
    if !Debug.debug_resolve then !!% "Unshadow...@.";
    Unshadow.Alias.insert str
end

