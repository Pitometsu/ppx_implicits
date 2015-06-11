open List
open Utils

module Longident = struct
  include Longident
  let format = Pprintast.default#longident
  let to_string l = Format.ksprintf (fun x -> x) "%a" format l
end

module Ident = struct
  include Ident
  let format ppf id = Format.fprintf ppf "%s/%d" id.name id.stamp
end

module Path = struct
  include Path

  let rec format_verbose ppf =
    let open Format in
    function
      | Pident id -> Ident.format ppf id
      | Pdot (p, name, n) -> fprintf ppf "%a.%s__%d" format_verbose p name n
      | Papply (p1, p2) -> fprintf ppf "%a(%a)" format_verbose p1 format_verbose p2

  let rec format ppf =
    let open Format in
    function
      | Pident id -> Ident.format ppf id
      | Pdot (p, name, _n) -> fprintf ppf "%a.%s" format p name
      | Papply (p1, p2) -> fprintf ppf "%a(%a)" format p1 format p2
end
  
module Location = struct
  include Location
  let format = print_loc
end

module Types = struct
  include Types
  open Btype
  open Ctype
  let repr_desc ty = (repr ty).desc
  let expand_repr_desc env ty = (repr & expand_head env ty).desc

  let with_snapshot f =
    let snapshot = snapshot () in
    let res = protect f in
    backtrack snapshot;
    unprotect res

  let is_constr env ty = match expand_repr_desc env ty with
    | Tconstr (p, tys, _) -> Some (p, tys)
    | _ -> None
  
  let is_option_type env ty = match is_constr env ty with
    | Some (po, [ty]) when po = Predef.path_option -> Some ty
    | _ -> None

  let gen_vars ty =
    flip filter (Ctype.free_variables ty) & fun ty ->
      ty.level = Btype.generic_level

  (* Create a type which can be unified only with itself *)
  let create_uniq_type =
    let cntr = ref 0 in
    fun () -> 
      incr cntr;
      (* Ident.create is not good. Unifying this data type ident with
         a tvar may cause "escaping the scope" errors
      *)
      Ctype.newty ( Tconstr ( Pident (Ident.create_persistent & "*uniq*" ^ string_of_int !cntr), [], ref Mnil ) )

  (* Unify genvars with unique types *)
  let close_gen_vars ty =
    List.iter (fun gv ->
      match repr_desc gv with
      | Tvar _ ->
          Ctype.unify Env.empty gv (create_uniq_type ());
          (* eprintf "Closing %a@." Printtyp.type_expr gv *)
      | Tunivar _ -> ()
      | _ -> assert false) & gen_vars ty
end
