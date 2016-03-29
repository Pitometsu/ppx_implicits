open Ppxx.Utils
open List
open Typpx.Compilerlib
open Ident
open Path
open Typpx
(* open Format *)

let conv_ident id = "__" ^ id.name ^ "__" ^ string_of_int id.stamp

let rec check_module_path env path =
  (* !!% "  checking %a@." Path.format_verbose path; *)
  let lid = Untypeast.lident_of_path path in
  let path' = try Some (Env.lookup_module ~load:true (* CR jfuruse: ? *) lid env) with _ -> None in 
  if Some path = path' then `Accessible lid
  else begin
  (* !!% "    shadowed: found %a@." (Option.format Path.format_verbose) path'; *)
    match path with
    | Pident id -> 
        let n = conv_ident id in
        let id' = Ident.create n in
        `Shadowed (id, id', Pident id')
    | Pdot (p, n, x) -> 
        begin match check_module_path env p with
        | `Shadowed (id, name, p) -> `Shadowed (id, name, Pdot (p, n, x))
        | `Accessible _ -> assert false (* impos *)
        | `Not_found p -> `Not_found p
    end
    | _ -> assert false (* impos *)
  end

let aliases = ref ([]: (Ident.t * Ident.t) list)
(* CR jfuruse: This is bizarre to have a ref which none of this module touches... *)

let reset () = aliases := []

module Replace = struct
  module MapArg = struct
    include TypedtreeMap.DefaultMapArgument
    
    open Typedtree
    
    module Forge = Forge
    
    let enter_expression e = match e.exp_desc with
      | Texp_ident (p, {loc}, vd) ->
          let env = e.exp_env in
          begin match p with
          | Path.Pdot (p, n, i) ->
              begin match check_module_path env p with
              | `Accessible _ -> e
              | `Not_found _p ->
                  !!% "ppx_implicits: unshadow: module path %a not found in the env@." Path.format p; 
                  assert false (* impos *)
              | `Shadowed (id, id', p) ->
                  aliases := (id, id') :: !aliases;
                  let p = Path.Pdot (p, n, i) in
                  let lid = Untypeast.lident_of_path p in
                  { e with exp_desc = Texp_ident (p, {txt=lid; loc}, vd) }
              end
          | Path.Pident _ ->
              let lid = Untypeast.lident_of_path p in
              begin
                try
                  let p', _ = Env.lookup_value lid env in
                  assert (p = p');
                  e
                with Not_found -> e (* It is I guess __imp_function__ *)
              end
          | _ -> assert false (* impos *)
          end
      | _ -> e
  end
  
  module Map = TypedtreeMap.MakeMap(MapArg)

  let replace e = Map.map_expression e
end
  
module Alias = struct
  module MapArg(A : sig val aliases : (Ident.t * Ident.t) list end) : TypedtreeMap.MapArgument = struct
  
    (* Introduce module aliases to avoid shadow
  
       module M = ...
       
       =>
  
       module M___ = M
       module M = ...
  
       or
  
       let module M = ... in
       
       =>
  
       let module M___ = M in
       let module M = ... in
    *)
    include TypedtreeMap.DefaultMapArgument
  
    open Typedtree
  
    open Forge
  
    let enter_expression e = match e.exp_desc with
      | Texp_letmodule (id, a, b, e') ->
          begin match assoc_opt id A.aliases with
          | None -> e
          | Some id' -> 
              let p = Pident id in
              { e with exp_desc = 
                 Texp_letmodule (id, a, b, Exp.letmodule id' (Forge.Mod.ident p) e')
              }
          end
      | _ -> e
  
    let structure_item si = match si.str_desc with
      | Tstr_module mb ->
  (*
  !!% "module %a@." Ident.format mb.mb_id;
  *)
          si ::
          begin match assoc_opt mb.mb_id A.aliases with
          | None -> []
          | Some id' -> 
              let p = Pident mb.mb_id in
              [ { (Dummy.structure_item ())
                  with str_desc = Tstr_module (MB.module_binding id' & Forge.Mod.ident p)
                } ]
          end
      | Tstr_recmodule mbs ->
          si :: flip filter_map mbs (fun mb ->
            match assoc_opt mb.mb_id A.aliases with
            | None -> None
            | Some id' -> 
                let p = Pident mb.mb_id in
                Some { (Dummy.structure_item ())
                       with str_desc = Tstr_module (MB.module_binding id' & Forge.Mod.ident p)
                     })
      | _ -> [si]
  
    let enter_structure ({ str_items = sis } as s) =
      (* It ignores the correctness of type information *)
      { s 
        with str_items = fold_right (fun si st -> structure_item si @ st) sis [] }
  end
  
  let insert str =
    let module A = MapArg(struct let aliases = !aliases end) in
    let module Map = TypedtreeMap.MakeMap(A) in
    Map.map_structure str
end
