open Utils
open List
open Compilerlibx
open Ident
open Path
(* open Format *)

let conv_ident id = "__" ^ id.name ^ "__" ^ string_of_int id.stamp

let rec check_module_path env path =
(*
  eprintf "  checking %a@." Path.format_verbose path;
*)
  let lid = Untypeast.lident_of_path path in
  let path' = try Some (Env.lookup_module ~load:true (* CR jfuruse: ? *) lid env) with _ -> None in 
  if Some path = path' then `Accessible lid
  else begin
(*
    eprintf "    shadowed: found %a@." (Option.format Path.format_verbose) path';
*)
    match path with
    | Pident id -> 
        let n = conv_ident id in
        let id' = Ident.create n in
        `Shadowed (id, id', Pident id')
    | Pdot (p, n, x) -> 
        begin match check_module_path env p with
        | `Shadowed (id, name, p) -> `Shadowed (id, name, Pdot (p, n, x))
        | `Accessible _ -> assert false
        | `Not_found p -> `Not_found p
    end
    | _ -> assert false
  end

let check_module_path env path =
(*
  eprintf "check_module_path: %a@." Path.format path;
*)
  check_module_path env path
        
let aliases = ref ([] : (Ident.t * Ident.t) list)

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  open Typedtree
  open Forge

  let enter_expression e = match e.exp_desc with
    | Texp_letmodule (id, a, b, e') ->
        begin match assoc_opt id !aliases with
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
eprintf "module %a@." Ident.format mb.mb_id;
*)
        si ::
        begin match assoc_opt mb.mb_id !aliases with
        | None -> []
        | Some id' -> 
            let p = Pident mb.mb_id in
            [ { Dummy.structure_item 
                with str_desc = Tstr_module (MB.module_binding id' & Forge.Mod.ident p)
              } ]
        end
    | Tstr_recmodule mbs ->
        si :: flip filter_map mbs (fun mb ->
          match assoc_opt mb.mb_id !aliases with
          | None -> None
          | Some id' -> 
              let p = Pident mb.mb_id in
              Some { Dummy.structure_item 
                     with str_desc = Tstr_module (MB.module_binding id' & Forge.Mod.ident p)
                   })
    | _ -> [si]

  let enter_structure ({ str_items = sis } as s) =
    (* It ignores the correctness of type information *)
    { s 
      with str_items = fold_right (fun si st -> structure_item si @ st) sis [] }
end

module Map = TypedtreeMap.MakeMap(MapArg)
