(*  File based on Why3 Software:    *)
(*  why3/plugins/python/py_ast.mli  *)

type ident = string

type unop =
  | Uneg (* -e *)
  | Unot (* not e *)

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)

type expr = {
    expr_desc: expr_desc;
    expr_loc : Position.t
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
    stmt_loc : Position.t
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

type file = block

(* Pretty printers *)

val pp_expr : Format.formatter -> expr -> unit
val pp_stmt : Format.formatter -> stmt -> unit
val pp_decl : Format.formatter -> decl -> unit
val pp_file : Format.formatter -> file -> unit
val show_expr : expr -> string
val show_stmt : stmt -> string
val show_decl : decl -> string
val show_file : file -> string
