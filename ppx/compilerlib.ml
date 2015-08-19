(* Extension of compilier_libs modules *)

open List
open Utils

module Longident = struct
  include Longident

  open Format
    
  let format = Pprintast.default#longident
  let to_string l = ksprintf (fun x -> x) "%a" format l
end

module Ident = struct
  include Ident

  open Format
    
  let format ppf id = pp_print_string ppf id.name
  let format_verbose ppf id = fprintf ppf "%s/%d" id.name id.stamp
end

module Path = struct
  include Path

  open Format

  let rec format ppf = function
    | Pident id -> Ident.format ppf id
    | Pdot (p, name, _n) -> fprintf ppf "%a.%s" format p name
    | Papply (p1, p2) -> fprintf ppf "%a(%a)" format p1 format p2

  let rec format_verbose ppf = function
    | Pident id -> Ident.format_verbose ppf id
    | Pdot (p, name, n) -> fprintf ppf "%a.%s__%d" format_verbose p name n
    | Papply (p1, p2) -> fprintf ppf "%a(%a)" format_verbose p1 format_verbose p2

  let to_string l = ksprintf (fun x -> x) "%a" format l
end
  
module Location = struct
  include Location
  let format = print_loc
end

module XTypes : sig
  open Types
  val repr_desc : type_expr -> type_desc
  (** repr + desc *)
    
  val expand_repr_desc : Env.t -> type_expr -> type_desc
  (** expand_head + repr + desc *)
    
  val with_snapshot : (unit -> 'a) -> 'a
  (** Run the given function. Unifications caused by the function are undo-ed. *)
    
  val is_constr : Env.t -> type_expr -> (Path.t * type_expr list) option
  (** Check the type is a Tconstr *)
    
  val is_option_type : Env.t -> type_expr -> type_expr option
  (** Check the type is option *)
    
  val gen_vars : type_expr -> type_expr list
  (** Generalized tvars *)
    
  val create_uniq_type : unit -> type_expr
  (** Create a unique data type. Note that the result data type lacks definition. *)
    
  val close_gen_vars : type_expr -> unit
  (** Unify genvars with unique types *)
    
end  = struct
  open Types
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

  let close_gen_vars ty =
    List.iter (fun gv ->
      match repr_desc gv with
      | Tvar _ ->
          Ctype.unify Env.empty gv (create_uniq_type ());
          (* eprintf "Closing %a@." Printtyp.type_expr gv *)
      | Tunivar _ -> ()
      | _ -> assert false) & gen_vars ty
end

module Types = struct
  include Types
  include XTypes
end

(* Pity! This is version dependent *)
module BytecompOptions(A : sig
  val impl : string -> unit
  val intf : string -> unit
  val anonymous : string -> unit
end) = Main_args.Make_bytecomp_options (struct
  open Clflags
  open Compenv
  
  let show_config () =
    Config.print_config stdout;
    exit 0;
  ;;
  
  let set r () = r := true
  let unset r () = r := false
  let _a = set make_archive
  let _absname = set Location.absname
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _cc s = c_compiler := Some s
  let _cclib s = ccobjs := Misc.rev_split_words s @ !ccobjs
  let _ccopt s = first_ccopts := s :: !first_ccopts
  let _compat_32 = set bytecode_compatible_32
  let _config = show_config
  let _custom = set custom_runtime
(* 4.02.2 *)
  let _no_check_prims = set no_check_prims
  let _dllib s = dllibs := Misc.rev_split_words s @ !dllibs
  let _dllpath s = dllpaths := !dllpaths @ [s]
  let _for_pack s = for_package := Some s
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I s = include_dirs := s :: !include_dirs
  let _impl = A.impl
  let _intf = A.intf
  let _intf_suffix s = Config.interface_suffix := s
(* 4.02.2 *)
  let _keep_docs = set keep_docs
  let _keep_locs = set keep_locs
  let _labels = unset classic
  let _linkall = set link_everything
  let _make_runtime () =
    custom_runtime := true; make_runtime := true; link_everything := true
  let _no_alias_deps = set transparent_modules
  let _no_app_funct = unset applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noautolink = set no_auto_link
  let _nostdlib = set no_std_include
  let _o s = output_name := Some s
  let _open s = open_modules := s :: !open_modules
  let _output_obj () = output_c_object := true; custom_runtime := true
(* 4.02.2 *)
  let _output_complete_obj () =
    output_c_object := true; output_complete_object := true; custom_runtime := true
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _ppx s = first_ppx := s :: !first_ppx
  let _principal = set principal
  let _rectypes = set recursive_types
  let _runtime_variant s = runtime_variant := s
  let _safe_string = unset unsafe_string
  let _short_paths = unset real_paths
  let _strict_sequence = set strict_sequence
  let _strict_formats = set strict_formats
  let _thread = set use_threads
  let _vmthread = set use_vmthreads
  let _unsafe = set fast
  let _unsafe_string = set unsafe_string
  let _use_prims s = use_prims := s
  let _use_runtime s = use_runtime := s
  let _v () = print_version_and_library "compiler"
  let _version = print_version_string
  let _vnum = print_version_string
  let _w = (Warnings.parse_options false)
  let _warn_error = (Warnings.parse_options true)
  let _warn_help = Warnings.help_warnings
  let _where = print_standard_library
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr
  let anonymous = A.anonymous
end)