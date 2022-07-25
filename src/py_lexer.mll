(* Emacs, use -*- tuareg -*- to open this file. *)

(*  File based on Why3 Software:      *)
(*  why3/plugins/python/py_lexer.mll  *)

{
  open Lexing
  open Py_ast
  open Py_parser

  exception Lexing_error of string

  let id_or_kwd =
    let h = Hashtbl.create 32 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      ["def", DEF; "if", IF; "else", ELSE; "elif", ELIF;
       "return", RETURN; "while", WHILE;
       "for", FOR; "in", IN;
       "and", AND; "or", OR; "not", NOT;
       "True", TRUE; "False", FALSE; "None", NONE;
       "from", FROM; "import", IMPORT; "break", BREAK;
      ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENT s

  let string_buffer = Buffer.create 1024

  let stack = ref [0]  (* indentation stack *)
  let reset_stack () = stack := [0]

  let rec unindent n = match !stack with
    | m :: _ when m = n -> []
    | m :: st when m > n -> stack := st; END :: unindent n
    | _ -> raise (Lexing_error "bad indentation")

  let update_stack n =
    match !stack with
    | m :: _ when m < n ->
       stack := n :: !stack;
       [NEWLINE; BEGIN]
    | _ ->
       NEWLINE :: unindent n
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let integer = ['0'-'9']+
let space = ' ' | '\t'
let comment = "#" [^'\n']*

rule next_tokens = parse
  | '\n'    { new_line lexbuf; update_stack (indentation lexbuf) }
  | (space | comment)+
            { next_tokens lexbuf }
  | ident as id
            { [id_or_kwd id] }
  | '+'     { [PLUS] }
  | '-'     { [MINUS] }
  | '*'     { [TIMES] }
  | "//"    { [DIV] }
  | '%'     { [MOD] }
  | '='     { [EQUAL] }
  | "=="    { [CMP Beq] }
  | "!="    { [CMP Bneq] }
  | "<"     { [CMP Blt] }
  | "<="    { [CMP Ble] }
  | ">"     { [CMP Bgt] }
  | ">="    { [CMP Bge] }
  | '('     { [LEFTPAR] }
  | ')'     { [RIGHTPAR] }
  | '['     { [LEFTSQ] }
  | ']'     { [RIGHTSQ] }
  | ','     { [COMMA] }
  | ':'     { [COLON] }
  | integer as s
            { [INTEGER s] }
  | '"'     { [STRING (string lexbuf)] }
  | eof     { [EOF] }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

(* count the indentation, i.e. the number of space characters from bol *)
and indentation = parse
  | (space | comment)* '\n'
      (* skip empty lines *)
      { new_line lexbuf; indentation lexbuf }
  | space* as s
      { String.length s }

and string = parse
  | '"'
      { let s = Buffer.contents string_buffer in
        Buffer.reset string_buffer;
        s }
  | "\\n"
      { Buffer.add_char string_buffer '\n';
        string lexbuf }
  | "\\\""
      { Buffer.add_char string_buffer '"';
        string lexbuf }
  | _ as c
      { Buffer.add_char string_buffer c;
        string lexbuf }
  | eof
      { raise (Lexing_error "unterminated string") }

{

  let next_token =
    let tokens = Queue.create () in
    fun lb ->
    if Queue.is_empty tokens then begin
        let l = next_tokens lb in
        List.iter (fun t -> Queue.add t tokens) l
      end;
    Queue.pop tokens

}
