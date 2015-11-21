(* Main *)

open Ast_mapper

module M = Typpx.Make.F(struct
  let tool_name = "ppx_implicit"
  let args = []
  let firstUntypedTransformation = Pre.extend default_mapper
  module Typemod = Typpx.Default.Typemod
  module TypedTransformation = Mod.Map
  let lastUntypedTransformation = Typpx.Default.untyped_identity
end)

let () = M.run ()
