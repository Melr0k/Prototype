(* Emacs, use -*- tuareg -*- to open this file. *)

(*  File based on Why3 Software:       *)
(*  why3/plugins/python/py_parser.mly  *)

%{
  open Common
  open Py_ast

  let floc = Position.lex_join
  let mk_id id _ _ = id
  let mk_expr loc d = { expr_desc = d; expr_loc = loc }
  let mk_stmt loc d = Dstmt { stmt_desc = d; stmt_loc = loc }
%}

%token <string> INTEGER
%token <string> STRING
%token <Py_ast.binop> CMP
%token <string> IDENT
%token DEF IF ELSE ELIF RETURN WHILE FOR IN AND OR NOT NONE TRUE FALSE
%token FROM IMPORT BREAK
%token EOF
%token LEFTPAR RIGHTPAR LEFTSQ RIGHTSQ COMMA EQUAL COLON BEGIN END NEWLINE
%token PLUS MINUS TIMES DIV MOD

(* precedences *)

(*
%nonassoc IN
%nonassoc ELSE
 *)
%right OR
%right AND
%nonassoc NOT
%right CMP
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc unary_minus
%nonassoc LEFTSQ

%start file

%type <Py_ast.file> file
%type <Py_ast.decl> stmt

%%

file:
| NEWLINE* EOF
    { [] }
| NEWLINE? dl=nonempty_list(decl) NEWLINE? EOF
    { dl }
;

decl:
| import { $1 }
| def    { $1 }
| stmt   { $1 }

import:
| FROM m=ident IMPORT l=separated_list(COMMA, ident) NEWLINE
  { Dimport (m, l) }

def:
| DEF f = ident LEFTPAR x = separated_list(COMMA, ident) RIGHTPAR
  COLON NEWLINE BEGIN l=nonempty_list(stmt) END
    { Ddef (f, x, l) }
;

expr:
| d = expr_desc
   { mk_expr (floc $startpos $endpos) d }
;

expr_desc:
| NONE
    { Enone }
| TRUE
    { Ebool true }
| FALSE
    { Ebool false }
| c = INTEGER
    { Eint c }
| s = STRING
    { Estring s }
| id = ident
    { Eident id }
| e1 = expr LEFTSQ e2 = expr RIGHTSQ
    { Eget (e1, e2) }
| MINUS e1 = expr %prec unary_minus
    { Eunop (Uneg, e1) }
| NOT e1 = expr
    { Eunop (Unot, e1) }
| e1 = expr o = binop e2 = expr
    { Ebinop (o, e1, e2) }
| e1 = expr TIMES e2 = expr
    { match e1.expr_desc with
      | Elist [e1] -> Emake (e1, e2)
      | _ -> Ebinop (Bmul, e1, e2) }
| f = ident LEFTPAR e = separated_list(COMMA, expr) RIGHTPAR
    { Ecall (f, e) }
| LEFTSQ l = separated_list(COMMA, expr) RIGHTSQ
    { Elist l }
| LEFTPAR e = expr RIGHTPAR
    { e.expr_desc }
;

%inline binop:
| PLUS  { Badd }
| MINUS { Bsub }
| DIV   { Bdiv }
| MOD   { Bmod }
| c=CMP { c    }
| AND   { Band }
| OR    { Bor  }
;

located(X):
| X { mk_stmt (floc $startpos $endpos) $1 }
;

suite:
| s = simple_stmt NEWLINE
    { [s] }
| NEWLINE BEGIN l = nonempty_list(stmt) END
    { l }
;

stmt:
| located(stmt_desc)      { $1 }
| s = simple_stmt NEWLINE { s }

stmt_desc:
| IF c = expr COLON s1 = suite s2=else_branch
    { Sif (c, s1, s2) }
| WHILE e = expr COLON s=suite
    { Swhile (e, s) }
| FOR x = ident IN e = expr COLON s=suite
    { Sfor (x, e, s) }
;

else_branch:
| /* epsilon */
    { [] }
| ELSE COLON s2=suite
    { s2 }
| ELIF c=expr COLON s1=suite s2=else_branch
    { [mk_stmt (floc $startpos $endpos) (Sif (c, s1, s2))] }

simple_stmt: located(simple_stmt_desc) { $1 };

simple_stmt_desc:
| RETURN e = expr
    { Sreturn e }
| id = ident EQUAL e = expr
    { Sassign (id, e) }
| e1 = expr LEFTSQ e2 = expr RIGHTSQ EQUAL e3 = expr
    { Sset (e1, e2, e3) }
| e = expr
    { Seval e }
| BREAK
    { Sbreak }
;

ident:
  id = IDENT { mk_id id $startpos $endpos }
;
