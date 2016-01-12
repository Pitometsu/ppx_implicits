(*

  Instance search space specification DSL, magling to and back from
  OCaml type definitions.

*)
open Ppxx.Utils
open List

open Ppxx.Compilerlib
open Longident
open Types

open Typpx

open Asttypes

open Candidate

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
