open Typedtree

val check_module_path 
      : Env.t 
      -> Path.t 
      -> [ `Accessible of Longident.t
         | `Not_found of Path.t
         | `Shadowed of Ident.t * Compilerlibx.Ident.t * Path.t ]

val aliases : (Ident.t * Ident.t) list ref

module Map : sig
  val map_structure      : structure -> structure
  val map_pattern        : pattern -> pattern
  val map_structure_item : structure_item -> structure_item
  val map_expression     : expression -> expression
  val map_class_expr     : class_expr -> class_expr
  val map_signature      : signature -> signature
  val map_signature_item : signature_item -> signature_item
  val map_module_type    : module_type -> module_type
end
