(*
[%%imp derive.show: 'a -> string] : int -> string

=>  [%derive.show: int]
*)

open Utils
open Ppxx.Utils
open Ppxx.Compilerlib
open Typedtree
open Types

(* CR jfuruse: this is crazy *)      
let fake_path e = 
  let open Parsetree in
  Path.Pident (Ident.create_persistent & Utils.mangle & Pprintast.string_of_expression e)

let instance_template e cty = 
  let open Ast_mapper in
  let open Parsetree in
  let extend super =
    let typ self ty = match ty.ptyp_desc with
      | Ptyp_any -> cty
      | _ -> super.typ self ty
    in
    { super with typ }
  in
  let mapper = extend default_mapper in
  mapper.expr mapper e

(*
      let {Typedtree.ctyp_type= temp_type} =
        try Typetexp.transl_simple_type env true cty with _ -> assert false (* CR jfuruse: better error *)
      in
      let temp_tvar = 
        match gen_vars temp_type, Ctype.free_variables temp_type with
        | [v], [v'] when v == v' -> v
        | _ -> assert false (* CR jfuruse: better error *)
      in
      { temp_path; temp_expr; temp_tvar; temp_type }
  | _ -> assert false (* CR jfuruse: better error *)
*)
    
let cand_derive env loc e temp_ty ty =
  let temp_tvar = match Ctype.free_variables temp_ty with
    | [tvar] -> tvar
    | _ -> assert false (* CR jfuruse: error handling *)
  in
  let ttvar, ttype = match Ctype.instance_list env [temp_tvar; temp_ty] with
    | [ttvar; ttype] -> ttvar, ttype
    | _ -> assert false
  in
  exit_then [] & fun () ->
    begin try Ctype.unify env ttype ty with Ctype.Unify _ -> raise Exit end;
    (* ttvar should be now unified.  It must be now closed. *)
    match Ctype.free_variables ttvar with
    | [] ->
        let ty_str = Format.asprintf "%a" Printtyp.type_scheme ttvar in

        Lexer.init ();
        let lexbuf = Lexing.from_string ty_str in
        let cty =
          try Parser.parse_core_type Lexer.token lexbuf
          with e ->
            Location.report_exception Format.err_formatter e;
            raise e
        in
        begin try
          let {ctyp_type} = Typetexp.transl_simple_type env true cty in
          Ctype.unify env ttvar ctyp_type
        with
        | _ ->
            warnf "@[<2>%a: deriving.xxx cannot be used as an instance since a path of the type %a is shadowned and inaccessible..@]"
              Location.format loc
              Printtyp.type_scheme ttvar;
            raise Exit
        end;
        let path = fake_path e in
        [ { Candidate.lid= Typpx.Untypeast.lident_of_path path;
            path= path;
            expr= Typpx.Forge.Exp.untyped (instance_template e cty);
            type_= ttype;
            aggressive = false } ]
    | _ -> []
    
  
