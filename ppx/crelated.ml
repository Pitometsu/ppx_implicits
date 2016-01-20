open Ppxx.Utils
open List

open Ppxx.Compilerlib
open Types

open Candidate

let data_types env ty =
  let open Btype in
  let open Ctype in
  let res = ref [] in
  (* CR jfuruse: oops, may loop forever? *)
  let expand_repr_desc env ty = (repr & expand_head env ty).desc in
  let rec loop ty = 
    begin match expand_repr_desc env ty with
    | Tconstr (p, _tys, _) ->
        res := p :: !res;
    | _ -> ()
    end;
    iter_type_expr loop ty
  in
  loop ty;
  sort_uniq compare !res

let related_modules env ty =
  let open Path in
  data_types env ty
  |> filter_map (function
      | Pdot (p, _, _) -> Some p
      | Pident _ -> None
      | Papply _ -> assert false)
  |> sort_uniq compare

(* CR jfuruse: loc is not used *)    
let cand_related env _loc ty = 
  let mods = related_modules env ty in
  let lmods = flip map mods & fun p ->
    let mdecl = Env.find_module p env in
    (Typpx.Untypeast.lident_of_path p, p, mdecl)
  in
  (* CR jfuruse: values_of_module should be memoized *)
  concat & map (fun (lid,path,mdecl) -> values_of_module ~recursive:false env lid path mdecl) lmods

