val extend : Ast_mapper.mapper -> Ast_mapper.mapper

module TypeClass : sig
  open Parsetree

  val process_module_type_declaration : module_type_declaration -> structure_item
end
