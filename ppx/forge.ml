(* Forging typed AST

   This is *not* to create a well-typed Typedtree.
   This is only to forge a Typedtree AST which will be untypeast'ed 
   to Parsetree immediately.
*)
open Utils
open Typedtree

module Dummy = struct

  open Types

  let type_expr = Btype.newgenty (Types.Tvar None)

  let env = Env.empty

  let value_description = 
    { val_type       = type_expr;
      val_kind       = Val_reg;
      val_loc        = Location.none;
      val_attributes = [] 
    }

  let exp_desc = Texp_tuple []

  let exp = 
    { exp_desc;
      exp_loc        = Location.none;
      exp_extra      = [];
      exp_type       = type_expr;
      exp_env        = env;
      exp_attributes = [] 
    }

  let mod_type = Mty_signature [] 

  let structure_item = { str_desc = Tstr_recmodule []
                       ; str_loc = Location.none
                       ; str_env = env 
                       }
end

let loc txt = 
  let open Location in
  { loc = none; txt }

let lidentloc_of_path p = loc & Untypeast.lident_of_path p

module Path = struct
  open Longident
  open Path
  let rec of_lident = function
    | Lident s -> Pident (Ident.create s)
    | Ldot (t,s) -> Pdot (of_lident t, s, 0)
    | Lapply (t1,t2) -> Papply (of_lident t1, of_lident t2)
end

module Exp = struct

  let ident lid p = 
    { Dummy.exp with
      exp_desc = Texp_ident (p, loc lid, Dummy.value_description) } 

  let let_ ?(recursive=false) vbs e =
    { Dummy.exp with
      exp_desc = Texp_let((if recursive then Recursive else Nonrecursive),
                          vbs,
                          e)
    }

  let letmodule id mexpr e =
    { Dummy.exp with 
      exp_desc = Texp_letmodule (id, loc (Ident.name id), mexpr, e) }

  let app e les =
    match les with
    | [] -> e
    | _ ->
        { Dummy.exp with
          exp_desc = Texp_apply(e, List.map (fun (l,e) -> l, Some e, Required (*?*)) les)
        }

  let some e =
    { Dummy.exp with
      exp_desc = Texp_construct ( loc (Longident.Lident "Some"),
                                 { Types.cstr_name = "Some"
                                 ; cstr_res = Dummy.type_expr
                                 ; cstr_existentials = []
                                 ; cstr_args = []
                                 ; cstr_arity = 1
                                 ; cstr_tag = Cstr_block 0
                                 ; cstr_consts = 1
                                 ; cstr_nonconsts = 1
                                 ; cstr_normal = 0
                                 ; cstr_generalized = false
                                 ; cstr_private = Public
                                 ; cstr_loc = Location.none
                                 ; cstr_attributes = [] },
                                 [e]) }
end

module Pat = struct

  let desc ?(loc=Location.none) d = 
    { pat_desc = d;
      pat_loc = loc;
      pat_extra = [];
      pat_type = Dummy.type_expr;
      pat_env = Dummy.env;
      pat_attributes = [];
    }

  let var id = desc (Tpat_var (id, loc (Ident.name id)))
end

module MB = struct
  let module_binding id x = { mb_id = id
                            ; mb_name = loc id.name
                            ; mb_expr = x
                            ; mb_attributes = []
                            ; mb_loc = Location.none 
                            } 
end

module Mod = struct
  let of_module_expr_desc d = 
    { mod_desc = d;
      mod_loc = Location.none;
      mod_type = Dummy.mod_type;
      mod_env = Dummy.env;
      mod_attributes = [] }

  let ident p = of_module_expr_desc & Tmod_ident (p, lidentloc_of_path p)

  let unpack e = of_module_expr_desc & Tmod_unpack (e, Dummy.mod_type)
end
