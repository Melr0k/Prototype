open Source.Ast
open Python.Py_ast

(* val parse_file : ('a -> Lexing.lexbuf -> 'b) -> 'a -> string -> 'b *)
(* val parse_string : ('a -> Lexing.lexbuf -> 'b) -> 'a -> string -> 'b *)

val parse_expr_file : string -> parser_expr
val parse_expr_string : string -> parser_expr

val parse_defs_file : string -> (string * parser_expr) list
val parse_defs_string : string -> (string * parser_expr) list

val parse_program_file : string -> parser_program
val parse_program_string : string -> parser_program

val parse_py_file : string -> file
val parse_py_string : string -> file

val std_fmt : Format.formatter ref
val err_fmt : Format.formatter ref
val wrn_fmt : Format.formatter ref
