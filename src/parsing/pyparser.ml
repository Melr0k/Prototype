open Ast
(* open Types.Additions *)
module PC = PyreAst.Concrete

(** temp type **)
type rtype = Ast.parser_expr
  (* varname * (annotation,type_expr, varname) Ast.t *)
  (* var     * (annotation * ast) *)

let no_var = "_"
let args_var = "_args_ "

let pos_of_loc ?(filename="<unknown>") ?(bol=0) ({start;stop}:PC.Location.t) =
  let pypos_to_lexpos ({line;column}:PC.Position.t) : Lexing.position =
    { pos_fname = filename
    ; pos_lnum = line
    ; pos_bol = bol
    ; pos_cnum = bol + column
    }
  in
  Position.lex_join (pypos_to_lexpos start) (pypos_to_lexpos stop)

(** Assume that && and || are defined in source language **)
let var_of_boolop (op:PC.BooleanOperator.t) : ('a,'b,'c) Ast.ast =
  Var (match op with And -> "&&" | Or -> "||")

let rec unfold_args filename (args:PC.Arguments.t) body = match args with
  | {posonlyargs; args; vararg; kwonlyargs; kw_defaults; kwarg; defaults} ->
     (** [PEP 570]    ⋅⋅--defaults------           kw_defaults
                            |                           |
                   ---------------------           ----------
         def f(pos1, pos2, /, pos_or_kwd, *vararg, kwd1, kwd2, **kwarg):
               -----------    ----------           ----------
                 |               |                      |
                 |     Positional or keyword (args)     |
       Positional only (posonlyargs)                 Keyword only (kwonlyargs)
      **)
     ignore (filename,posonlyargs,args,vararg,kwonlyargs,kw_defaults,kwarg,
             defaults,body);
     failwith "TODO"

and expr_to_t filename (expr:PC.Expression.t) : rtype =
  match expr with
  | BoolOp {location; op; values} ->
     let an = pos_of_loc ~filename location |> Ast.new_annot in
     let op2 = an, var_of_boolop op in
     begin match values with
     | [] -> op2
     | x::l ->
        let op1 = an, App (op2, expr_to_t filename x) in
        match l with
        | [] -> op1
        | y::l ->
           let ini = an, App (op1, expr_to_t filename y) in
           List.fold_left
             (fun b e -> an, App ( (an, App(op2, b))
                                  , expr_to_t filename e) )
             ini l
     end
  | NamedExpr {location;target;value}
    -> ignore (location,target,value); failwith "No translation yet"
  | BinOp {location;left;op;right}
    -> ignore (location,left,op,right); failwith "No translation yet"
  | UnaryOp {location;op;operand}
    -> ignore (location,op,operand); failwith "No translation yet"
  | Lambda {location; args; body} ->
     let an = pos_of_loc ~filename location |> Ast.new_annot in
     let body = expr_to_t filename body |> unfold_args filename args in
     an, Lambda (Unnanoted, args_var, body)
  | IfExp {location;test;body;orelse}
    -> ignore (location,test,body,orelse); failwith "No translation yet"
  | Dict {location;keys;values}
    -> ignore (location,keys,values); failwith "No translation yet"
  | Set {location;elts}
    -> ignore (location,elts); failwith "No translation yet"
  | ListComp {location;elt;generators}
    -> ignore (location,elt,generators); failwith "No translation yet"
  | SetComp {location;elt;generators}
    -> ignore (location,elt,generators); failwith "No translation yet"
  | DictComp {location;key;value;generators}
    -> ignore (location,key,value,generators); failwith "No translation yet"
  | GeneratorExp {location;elt;generators}
    -> ignore (location,elt,generators); failwith "No translation yet"
  | Await {location;value}
    -> ignore (location,value); failwith "No translation yet"
  | Yield {location;value}
    -> ignore (location,value); failwith "No translation yet"
  | YieldFrom {location;value}
    -> ignore (location,value); failwith "No translation yet"
  | Compare {location;left;ops;comparators}
    -> ignore (location,left,ops,comparators); failwith "No translation yet"
  | Call {location;func;args;keywords}
    -> ignore (location,func,args,keywords); failwith "No translation yet"
  | FormattedValue {location;value;conversion;format_spec}
    -> ignore (location,value,conversion,format_spec);
       failwith "No translation yet"
  | JoinedStr {location;values}
    -> ignore (location,values); failwith "No translation yet"
  | Constant {location;value;kind}
    -> ignore (location,value,kind); failwith "No translation yet"
  | Attribute {location;value;attr;ctx}
    -> ignore (location,value,attr,ctx); failwith "No translation yet"
  | Subscript {location;value;slice;ctx}
    -> ignore (location,value,slice,ctx); failwith "No translation yet"
  | Starred {location;value;ctx}
    -> ignore (location,value,ctx); failwith "No translation yet"
  | Name {location;id;ctx}
    -> ignore (location,id,ctx); failwith "No translation yet"
  | List {location;elts;ctx}
    -> ignore (location,elts,ctx); failwith "No translation yet"
  | Tuple {location;elts;ctx}
    -> ignore (location,elts,ctx); failwith "No translation yet"
  | Slice {location;lower;upper;step}
    -> ignore (location,lower,upper,step); failwith "No translation yet"

and stmt_to_t filename (stmt:PC.Statement.t) : rtype
  = match stmt with
  | FunctionDef {location;name;args;body;decorator_list;returns;
                 type_comment;type_params}
    -> ignore (location,name,args,body,decorator_list,returns,
               type_comment,type_params);
       failwith "No translation yet"
  | AsyncFunctionDef {location;name;args;body;decorator_list;returns;
                      type_comment;type_params}
    -> ignore (location,name,args,body,decorator_list,returns,
               type_comment,type_params);
       failwith "No translation yet"
  | ClassDef {location;name;bases;keywords;body;decorator_list;type_params}
    -> ignore (location,name,bases,keywords,body,decorator_list,type_params);
       failwith "No translation yet"
  | Return {location;value}
    -> ignore (location,value); failwith "No translation yet"
  | Delete {location;targets}
    -> ignore (location,targets); failwith "No translation yet"
  | Assign {location;targets;value;type_comment}
    -> ignore (location,targets,value,type_comment);
       failwith "No translation yet"
  | TypeAlias {location;name;type_params;value}
    -> ignore (location,name,type_params,value);
       failwith "No translation yet"
  | AugAssign {location;target;op;value}
    -> ignore (location,target,op,value); failwith "No translation yet"
  | AnnAssign {location;target;annotation;value;simple}
    -> ignore (location,target,annotation,value,simple);
       failwith "No translation yet"
  | For {location;target;iter;body;orelse;type_comment}
    -> ignore (location,target,iter,body,orelse,type_comment);
       failwith "No translation yet"
  | AsyncFor {location;target;iter;body;orelse;type_comment}
    -> ignore (location,target,iter,body,orelse,type_comment);
       failwith "No translation yet"
  | While {location;test;body;orelse}
    -> ignore (location,test,body,orelse); failwith "No translation yet"
  | If {location;test;body;orelse}
    -> ignore (location,test,body,orelse); failwith "No translation yet"
  | With {location;items;body;type_comment}
    -> ignore (location,items,body,type_comment); failwith "No translation yet"
  | AsyncWith {location;items;body;type_comment}
    -> ignore (location,items,body,type_comment); failwith "No translation yet"
  | Match {location;subject;cases}
    -> ignore (location,subject,cases); failwith "No translation yet"
  | Raise {location;exc;cause}
    -> ignore (location,exc,cause); failwith "No translation yet"
  | Try {location;body;handlers;orelse;finalbody}
    -> ignore (location,body,handlers,orelse,finalbody);
       failwith "No translation yet"
  | TryStar {location;body;handlers;orelse;finalbody}
    -> ignore (location,body,handlers,orelse,finalbody);
       failwith "No translation yet"
  | Assert {location;test;msg}
    -> ignore (location,test,msg); failwith "No translation yet"
  | Import {location;names}
    -> ignore (location,names); failwith "No translation yet"
  | ImportFrom {location;module_;names;level}
    -> ignore (location,module_,names,level); failwith "No translation yet"
  | Global {location;names}
    -> ignore (location,names); failwith "No translation yet"
  | Nonlocal {location;names}
    -> ignore (location,names); failwith "No translation yet"
  | Expr {location;value}
    -> ignore location; (* ? *)
       expr_to_t filename value
  | Pass {location}
    -> ignore (location); failwith "No translation yet"
  | Break {location}
    -> ignore (location); failwith "No translation yet"
  | Continue {location}
    -> ignore (location); failwith "No translation yet"

let py_to_t ?(filename="<unknown>") ({ body; _ }:PC.Module.t) :Ast.parser_expr =
  List.fold_left
    (fun ac stmt ->
      let v,e = no_var, stmt_to_t filename stmt in
      (Ast.new_annot Position.dummy, Let (v,e,ac)) ) (* other way around *)
    (Ast.new_annot Position.dummy,Const Nil)
    body

let parse filename =
  let open PyreAst.Parser in
  with_context (fun context ->
      match Concrete.parse_module ~context filename with
      | Result.Error { Error.message; line; column; _ } ->
         let message =
           Format.sprintf "@{<bold>@{<red>Parsing error@}: \
                           line %d, column %d:@} %s"
             line column message
         in
         failwith message
      | Result.Ok ast -> ast
    )
