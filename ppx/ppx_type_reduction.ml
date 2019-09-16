(* Main *)

open Ast_mapper

include Typpx.Make.F(struct
  let tool_name = "ppx_implicit"
  let args = [ "-debug-resolve", Arg.Set Debug.debug_resolve, "Ppx_implicits: resolution debug"
             ; "-debug-unif", Arg.Set Debug.debug_unif, "Ppx_implicits: unification debug"
             ]
  (* let firstUntypedTransformation = Pre.extend default_mapper *)
  let firstUntypedTransformation = Typpx.Default.untyped_identity
  module Typemod = Typpx.Default.Typemod
  module Config = struct
    let exp_function_enabled = false
    let final_check_enabled = false
  end
  module TypedTransformation = Mod.Map(Config)
  let lastUntypedTransformation = Typpx.Default.untyped_identity
end)
