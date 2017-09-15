(*

  Pre-preprocessing for syntax sugars for 
    [val %imp x : t]
*)

open Ppxx.Helper
open Parsetree
open Asttypes
open Ast_mapper

(* We use a primitive name "%IMP" of capital letters to emphasize it is not a real OCaml primitive. *)

let extend super = 
  let structure_item self sitem = match sitem.pstr_desc with
    | Pstr_extension ( ({txt="imp"}, PStr [{pstr_desc= Pstr_primitive vd}] ), _attr ) ->
       (* val%overloaded x : t  =>  external x : t = "%IMP" *)
       Str.primitive ~loc:sitem.pstr_loc { vd with pval_prim = [ "%IMP" ] }
    | _ -> super.structure_item self sitem
  in    
  let signature_item self sitem = match sitem.psig_desc with
    | Psig_extension ( ({txt="imp"}, PSig [{psig_desc= Psig_value vd}] ), _attr ) ->
       (* val%overloaded x : t  =>  external x : t = "%IMP" *)
       Sig.value ~loc:sitem.psig_loc { vd with pval_prim = [ "%IMP" ] }
    | _ -> super.signature_item self sitem
  in    
  { super with signature_item; structure_item }
