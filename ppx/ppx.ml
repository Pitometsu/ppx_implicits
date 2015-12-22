(* Main *)

open Ast_mapper

module M = Typpx.Make.F(struct
  let tool_name = "ppx_implicit"
  let args = [ "-debug-resolve", Arg.Set Options.debug_resolve, "Ppx_implicits: resolution debug"
             ; "-debug-unif", Arg.Set Options.debug_unif, "Ppx_implicits: unification debug"
             ]
  let firstUntypedTransformation = Pre.extend default_mapper
  module Typemod = Typpx.Default.Typemod
  module TypedTransformation = Mod.Map
  let lastUntypedTransformation = Typpx.Default.untyped_identity
end)

let () = M.run ()
