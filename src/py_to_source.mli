open Common
open Source
open Python

exception SyntaxError of Position.t * string (* pos, msg *)
exception Undefined of Position.t * string (* pos, var *)

val translate : Py_ast.file -> Ast.parser_program
val translate_input :
  [ `File of string | `String of string ] -> Ast.parser_program
