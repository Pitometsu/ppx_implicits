(*

  Pre-preprocessing for syntax sugars for 
    [val %imp x : t]

  to
    [external x : t = "%IMP"
     type __imp__x = t
    ]
*)

open Ppxx.Utils
open Ppxx.Helper
open Parsetree
open Asttypes
open Ast_mapper

(* We use a primitive name "%IMP" of capital letters to emphasize it is not a real OCaml primitive. *)

let extend super = 
  let structure self ss = flip List.concat_map ss & fun sitem ->
    match sitem.pstr_desc with
    | Pstr_extension ( ({txt="imp"}, PStr [{pstr_desc= Pstr_primitive vd}] ), _attr ) ->
       [ (* val%overloaded x : t  =>  external x : t = "%IMP" *)
         Str.primitive ~loc:sitem.pstr_loc { vd with pval_prim = [ "%IMP" ] }
       ; (* let __imp__x : t = Ppx_implicits.imp *)
         Str.value ~loc:sitem.pstr_loc Nonrecursive [
             Vb.mk ~loc:sitem.pstr_loc 
               (Pat.(constraint_
                       ~loc:vd.pval_loc
                       (var ~loc:vd.pval_loc { vd.pval_name with txt = "__Ppx_implicits_imp__"^ vd.pval_name.txt })
                       vd.pval_type))
               [%expr Ppx_implicits.imp]
           ]
       ]
    | _ -> [ super.structure_item self sitem ] 
  in    
  let signature self ss = flip List.concat_map ss & fun sitem ->
    match sitem.psig_desc with
    | Psig_extension ( ({txt="imp"}, PSig [{psig_desc= Psig_value vd}] ), _attr ) ->
       (* val%overloaded x : t  =>  external x : t = "%IMP" *)
       [ Sig.value ~loc:sitem.psig_loc { vd with pval_prim = [ "%IMP" ] }
       ; 
       ]
    | _ -> [ super.signature_item self sitem ]
  in    
  { super with signature; structure }
