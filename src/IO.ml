open Source
open Python

let parse_file parse_hook lex_tok fname =
  let buf = open_in fname |> Lexing.from_channel in
  buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = fname };
  parse_hook lex_tok buf

let parse_string parse_hook lex_tok str =
  let buf = Lexing.from_string str in
  buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = "_" };
  parse_hook lex_tok buf

let parse_expr_file =
  parse_file   Parser.unique_term Lexer.token

let parse_expr_string =
  parse_string Parser.unique_term Lexer.token

let parse_defs_file =
  parse_file   Parser.definitions Lexer.token

let parse_defs_string =
  parse_string Parser.definitions Lexer.token

let parse_program_file =
  parse_file   Parser.program Lexer.token

let parse_program_string =
  parse_string Parser.program Lexer.token

let py_parse a b =
  Py_lexer.reset_stack ();
  Py_parser.file a b

let parse_py_file =
  parse_file   py_parse Py_lexer.next_token

let parse_py_string =
  parse_string py_parse Py_lexer.next_token
