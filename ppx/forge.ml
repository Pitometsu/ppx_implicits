(* Forging typed AST

   This is *not* to create a well-typed Typedtree.
   This is only to forge a Typedtree AST which will be untypeast'ed 
   to Parsetree.
*)

module Dummy = struct

  open Types
  open Typedtree

  let type_expr = Btype.newgenty (Types.Tvar None)

  let env = Env.empty

  let value_description = 
    { val_type = type_expr;
      val_kind = Val_reg;
      val_loc = Location.none;
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
end

let loc txt = 
  let open Location in
  { loc = none; txt }

module Exp = struct
  open Typedtree

  let ident p = 
    { Dummy.exp with
      exp_desc = Texp_ident (p, loc (Untypeast.lident_of_path p), Dummy.value_description) } 

  let let_ ?(recursive=false) vbs e =
    { Dummy.exp with
      exp_desc = Texp_let((if recursive then Recursive else Nonrecursive),
                          vbs,
                          e)
    }

  let letmodule id mexpr e =
    { Dummy.exp with 
      exp_desc = Texp_letmodule (id, loc (Ident.name id), mexpr, e) }

end

module Pat = struct
  open Typedtree
  let var id = { pat_desc = Tpat_var (id, loc (Ident.name id));
                 pat_loc = Location.none;
                 pat_extra = [];
                 pat_type = Dummy.type_expr;
                 pat_env = Dummy.env;
                 pat_attributes = [];
               }
end

module Mod = struct
  open Typedtree

  let unpack e = 
    { mod_desc = Tmod_unpack (e, Dummy.mod_type);
      mod_loc = Location.none;
      mod_type = Dummy.mod_type;
      mod_env = Dummy.env;
      mod_attributes = [] }
end
