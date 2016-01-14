(*
[%%imp derive.show: 'a -> string] : int -> string

=>  [%derive.show: int]
*)

open Utils
open Ppxx.Utils
open Ppxx.Compilerlib
open Typedtree
open Types

type t = {
  temp_path : Path.t;
  temp_expr : Parsetree.core_type -> Parsetree.expression;
  temp_tvar : type_expr; (* The type variable appear in temp_type *)
  temp_type : type_expr;
}

(* (xxx : ty) *)
let parse0 p =
  let open Parsetree in
  match p.pexp_desc with
  | Pexp_constraint (e, cty) -> e, cty
  | _ -> assert false (* CR jfuruse: better error *)
      
(* (xxx : ty) *)
let parse env p =
  let open Parsetree in
  (* CR jfuruse: this is crazy *)      
  let temp_path = Path.Pident (Ident.create_persistent & Utils.mangle & Pprintast.string_of_expression p) in 
  match p.pexp_desc with
  | Pexp_constraint (e, cty) ->
      let temp_expr cty =
        let open Ast_mapper in
        let extend super =
          let typ self ty = match ty.ptyp_desc with
            | Ptyp_any -> cty
            | _ -> super.typ self ty
          in
          { super with typ }
        in
        let mapper = extend default_mapper in
        mapper.expr mapper e
      in
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
      
let cand_derive env loc temp ty =
  let ttvar, ttype = match Ctype.instance_list env [temp.temp_tvar; temp.temp_type] with
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
        [ { Candidate.lid= Typpx.Untypeast.lident_of_path temp.temp_path;
            path= temp.temp_path;
            expr= Typpx.Forge.Exp.untyped (temp.temp_expr cty);
            type_= ttype;
            aggressive = false } ]
    | _ -> []
    
  
