(*  File based on Why3 Software:   *)
(*  why3/plugins/python/py_ast.ml  *)

open Common
open Types_additions

type ident = string
[@@deriving show]

type unop =
  | Uneg (* -e *)
  | Unot (* not e *)
[@@deriving show]

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)
[@@deriving show]

type expr = {
    expr_desc: expr_desc;
    expr_loc : Position.t [@opaque]
  }

and expr_desc =
  | Enone
  | Ebool of bool
  | Eint of string
  | Estring of string
  | Eident of ident
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr
  | Ecall of ident * expr list
  | Elist of expr list   (* [e1, e2, ..., en] *)
  | Emake of expr * expr (* [e1] * e2 *)
  | Eget of expr * expr  (* e1[e2] *)

and stmt = {
    stmt_desc: stmt_desc;
    stmt_loc : Position.t [@opaque]
  }

and stmt_desc =
  | Sif of expr * block * block
  | Sreturn of expr
  | Sassign of ident * expr
  | Swhile of expr * block
  | Sfor of ident * expr * block
  | Seval of expr
  | Sset of expr * expr * expr (* e1[e2] = e3 *)
  | Sbreak

and block = decl list

and decl =
  | Dimport of ident * ident list
  | Ddef  of ident * ident list * block
  | Dstmt of stmt
[@@deriving show]

type file = block
[@@deriving show]

let basic_types =
  [ "bool", TBase TBool; "int", TBase (TInt (None, None));
    "string", TBase TString; "list", TBase TList;
    "function", TArrow (TBase TEmpty, TBase TAny);
     "type", TBase TAny;
    "float", TBase TAny;
    "NonType", TBase TAny;
    "builtin_function_or_method", TArrow (TBase TEmpty, TBase TAny)
  ]
