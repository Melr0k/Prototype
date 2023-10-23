open Ast
open Types.Additions

let parse file =
  let open PyreAst.Parser in
  with_context (fun context ->
      match Concrete.parse_module ~context file with
      | Result.Error { Error.message; line; column; end_line; end_column } ->
         ignore (end_line,end_column);
         let message =
           Format.sprintf "@{<bold>@{<red>Parsing error@}: \
                           line %d, column %d:@} %s"
             line column message
         in
         failwith message
      | Result.Ok ast -> ast
    )

(* let py_loc_to_pos (l:PyreAst.Concrete.Position.t) = match l with *)
(*   | {{sl;sc}; {el;ec}} -> *)
(*      let sp = Lexing.dummy_pos in *)
(*      let () = Lexing.set_position sp *)
(*      Ast.new_annot (Position.lex_join) *)
let py_stmt_to_t  (stmt:PyreAst.Concrete.Statement.t)
    : varname * (annotation, type_expr, varname) t
  = match stmt with
(*   | If {location; test; body; orelse} -> ( ,Ite (py_expr_to_t test) ) *)
  | _ -> failwith "test"

let py_to_t (py:PyreAst.Concrete.Module.t)
    : (annotation, type_expr, varname) Ast.t =
  List.fold_left
    (fun ac stmt ->
      let v,e = py_stmt_to_t stmt in
      (Ast.new_annot Position.dummy, Let (v,e,ac)) ) (* other way around *)
    (Ast.new_annot Position.dummy,Const Nil)
    py.body
