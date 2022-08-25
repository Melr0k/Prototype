open Source
open Python

(* val parse_file : ('a -> Lexing.lexbuf -> 'b) -> 'a -> string -> 'b *)
(* val parse_string : ('a -> Lexing.lexbuf -> 'b) -> 'a -> string -> 'b *)

val parse_expr_file : string -> Ast.parser_expr
val parse_expr_string : string -> Ast.parser_expr

val parse_defs_file : string -> (string * Ast.parser_expr) list
val parse_defs_string : string -> (string * Ast.parser_expr) list

val parse_program_file : string -> Ast.parser_program
val parse_program_string : string -> Ast.parser_program

val parse_py_file : string -> Py_ast.file
val parse_py_string : string -> Py_ast.file

val std_fmt : Format.formatter ref
val err_fmt : Format.formatter ref
val wrn_fmt : Format.formatter ref
