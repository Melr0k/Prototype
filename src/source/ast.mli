open Common

open Types_additions
open Variable
open Pomap

exception UndefinedSymbol of string
exception LexicalError of int * string
exception SyntaxError of string * string (* position * msg *)

type varname = string
type exprid = int

type annotation = exprid Position.located

type const =
  | Unit | Nil
  | EmptyRecord
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | Atom of string

type projection = Fst | Snd | Field of string

type 'typ type_annot = Unnanoted | ADomain of 'typ | AArrow of 'typ

type ('a, 'typ, 'v) ast =
  | Abstract of 'typ
  | Const of const
  | Var of 'v
  | Lambda of ('typ type_annot) * 'v * ('a, 'typ, 'v) t
  | Ite of ('a, 'typ, 'v) t * 'typ * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
  | App of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
  | Let of 'v * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
  | Pair of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
  | Projection of projection * ('a, 'typ, 'v) t
  (* RecordUpdate( ... (RecordUpdate(emptyrecord,x,Some xv),y,Some yv) ... ) *)
  | RecordUpdate of ('a, 'typ, 'v) t * string * ('a, 'typ, 'v) t option
  | Ref of ('a, 'typ, 'v) t
  | Read of ('a, 'typ, 'v) t
  | Assign of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
(*
  | Seq of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
 *)

and ('a, 'typ, 'v) t = 'a * ('a, 'typ, 'v) ast

type se = bool * bool (* side effects : (read, write) *)
type st_env = se VarMap.t (* var -> stable *)

type parser_expr = (annotation     , type_expr, varname   ) t
type annot_expr  = (annotation * se, Cduce.typ, Variable.t) t
type expr        = (             se, Cduce.typ, Variable.t) t

module Expr : Pomap_intf.PARTIAL_ORDER with type el = expr
module ExprMap : Pomap_intf.POMAP with type key = expr

type name_var_map = Variable.t StrMap.t
val empty_name_var_map : name_var_map

val unique_exprid : unit -> exprid
val identifier_of_expr : (annotation, 'a, 'b) t -> exprid
val position_of_expr : (annotation, 'a, 'b) t -> Position.t

(* Side-effects and stability *)
val no_se : se
val read_se : se
val write_se : se
val all_se : se
val (|||) : se -> se -> se
val (&&&) : se -> se -> se

val is_st_annot : annot_expr -> bool
val is_st_expr : expr -> bool

(* Ast transformations *)

val new_annot : Position.t -> annotation
val copy_annot : annotation -> annotation

val parser_expr_to_annot_expr : ?from_py:bool ->
  type_env -> var_type_env -> name_var_map -> st_env ->
  parser_expr -> annot_expr

(*val unannot : (VarSet.elt * bool) list -> annot_expr -> expr*)
val unannot_and_normalize : annot_expr -> expr
(*val fv : annot_expr -> VarSet.t*)
val substitute : annot_expr -> Variable.t -> annot_expr -> annot_expr

val const_to_typ : const -> Cduce.typ

type parser_element =
  | Definition of (bool (* log? *) * (string * parser_expr))
  | Atoms of string list
  | Types of (string * type_expr) list

type parser_program = parser_element list

(* Pretty printers *)

val pp_const : Format.formatter -> const -> unit
val pp_projection : Format.formatter -> projection -> unit
val pp_type_annot : (Format.formatter -> 'a -> unit) ->
                    Format.formatter -> 'a type_annot -> unit
val pp_ast : (Format.formatter -> 'a -> unit) ->
             (Format.formatter -> 'typ -> unit) ->
             (Format.formatter -> 'v -> unit) ->
             Format.formatter -> ('a, 'typ, 'v) ast -> unit
val pp_varname : Format.formatter -> varname -> unit

val show_parser_program : parser_program -> string
