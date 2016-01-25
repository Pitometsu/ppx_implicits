(*

  Pre-preprocessing for syntax sugars for 
    [%%imp_pack.t: <ty>] 

  =>

    type ('a,..) t = private <ty>
    module Instances = struct
      external pack : _d:<ty> -> ('a,..) t = "%identity"
      let pack_opt ~_d = Some (pack _d)
    end
*)

open Ppxx.Utils
open Utils
open Ppxx.Helper
open Ppxx.Compilerlib

open Parsetree
open Asttypes
open Ast_mapper
open List

let check_pack loc txt =
  let open Option.Monad in
  String.is_prefix "imp_pack." txt >>= fun s ->
  match expression_from_string s with
  | `Ok {pexp_desc= Pexp_ident {txt=Lident s}} -> Some s
  | _ ->
      errorf "%a: imp_pack.<id> must take a simple name for <id>"
        Location.format loc

(*
    type ('a,..) t = private <ty>
    module Instances = struct
      external pack : _d:<ty> -> ('a,..) t = "%identity"
      let pack_opt ~_d = Some (pack _d)
    end
*)
let build_type s tvars cty =
  Type.mk ~params:(map (fun x -> (x,Invariant)) tvars)
    ~priv: Private
    ~manifest: cty
    (at s)

let build_structure loc s cty = with_loc loc & fun () ->
  let tvars = map Typ.var & tvars_of_core_type cty in
  let res = Typ.constr (lid s) tvars in
  [ Str.type_ [ build_type s tvars cty ]
  ; [%stri
     module Instances = struct
       external pack : _d:[%t cty] -> [%t res] = "%identity"
       let pack_opt ~_d:(_d : [%t res]) = Some _d
     end
    ]
  ]

let build_signature loc s cty = with_loc loc & fun () ->
  let tvars = map Typ.var & tvars_of_core_type cty in
  let res = Typ.constr (lid s) tvars in
  [ Sig.type_ [ build_type s tvars cty ]
  ; sig_module_of_stri [%stri
     module type Instances = sig
       external pack : _d:[%t cty] -> [%t res] = "%identity"
       val pack_opt : _d:[%t res] -> [%t res] option
     end
    ]
  ]
  
(* type __imp_spec__ = private Spec_xxxx *)
let extend super =
  let structure self sitems =
    let sitems = flip concat_map sitems & fun sitem ->
      match sitem.pstr_desc with
      | Pstr_extension (({txt; loc}, PTyp cty), _) ->
          begin match check_pack loc txt with
          | None -> [ sitem ]
          | Some s -> build_structure sitem.pstr_loc s cty
          end
      | _ -> [ sitem ]
    in
    super.structure self sitems
  in
  
  let signature self sitems =
    let sitems = flip concat_map sitems & fun sitem ->
      match sitem.psig_desc with
      | Psig_extension (({txt; loc}, PTyp cty), _) ->
          begin match check_pack loc txt with
          | None -> [ sitem ]
          | Some s -> build_signature sitem.psig_loc s cty
          end
      | _ -> [ sitem ]
    in
    super.signature self sitems
  in

  { super with structure; signature }
