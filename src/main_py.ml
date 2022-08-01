open Common
open Source
open Python

open IO

exception FunArgs of Position.t * string * int * int
(* pos, fun, nbargs, given *)
exception Undefined of Position.t * string (* pos, var *)

let py_to_source py_ast =
  let module Py_env = Map.Make(String) in (* functions and their arguments *)

  let annot p ast = Ast.new_annot p, ast in
  let dannot ast = annot Position.dummy ast in (* semi-dummy annotation *)

  let argn = "__args__" in (* argument of all function *)
  let argvar = Ast.Var argn |> dannot in

  let is_ret (s:Py_ast.stmt) =
    match s.stmt_desc with Py_ast.Sreturn _ -> true | _ -> false in

  let rec make_lambda args body : Ast.parser_expr =
    (* build the arguments of Ast.Lambda with n arguments:
       def f(x, y, ...):
       =>
       λ argn. let x = argn.x in let y = argn.y in ...

       where argn = {x; y; z; ...}
     *)
    match args with
    | [] -> body
    | x::l ->
       let body' = make_lambda l body in
       Ast.(Let (x
                ,Projection (Field x, argvar) |> dannot
                ,body')) |> dannot
  in
  let make_app f loc varl astl =
    let rec aux n = function
      | []   , []    -> Ast.(Const EmptyRecord) |> annot loc
      | x::xl, a::al -> Ast.(RecordUpdate
                               (a ,x ,Some (aux (n+1) (xl,al))))
                        |> annot loc
      | _, [] -> FunArgs (loc, f, n + (List.length varl), n) |> raise
      | [], _ -> FunArgs (loc, f, n, n + (List.length astl)) |> raise
    in
    aux 0 (varl, astl)
  in

  let rec treat_expr env (e:Py_ast.expr) =
    let aux _ = function
      | Py_ast.Enone -> Ast.(Const Unit)
      | Py_ast.Eint str ->
         Ast.(Const (Int (int_of_string str)))
      | Py_ast.Ecall (fid, args) ->
         let argvars = match Py_env.find_opt fid env with
           | Some f -> f
           | None -> raise (Undefined (e.expr_loc,fid))
         in
         Ast.(App (Var fid |> annot e.expr_loc
                  , List.fold_left
                      (fun acc e -> treat_expr env e ::acc)
                      []
                      args
                    |> List.rev (* fold_right ? *)
                    |> make_app fid e.expr_loc argvars)
         )
      | _ -> failwith "TODO expr"
    in aux env e.expr_desc |> annot e.expr_loc
  in
  let rec treat_stmt env (s:Py_ast.stmt) = match s.stmt_desc with
    | Py_ast.Sif (t, e1, e2) ->
       let open Py_ast in
       begin match t.expr_desc with
       (* if (type(...) == ...): ...  =>  if ... is ... then ... *)
       | Ebinop (Beq
                ,{expr_desc=Ecall ("type", [e0]); expr_loc=_}
                ,{expr_desc=Estring typ; expr_loc=_}) ->
          Ast.Ite (treat_expr env e0
                  ,(match List.assoc_opt typ basic_types with
                      Some t -> t | _ -> TCustom typ)
                  ,treat_fun_decl env e1 |> snd
                  ,treat_fun_decl env e2 |> snd) |> annot s.stmt_loc
       | _ -> failwith "TODO normal if"
       end
     | Py_ast.Sreturn e | Py_ast.Seval e ->
       treat_expr env e (* |> annot s.stmt_loc *)
    | _ -> failwith "TODO stmt"
  and treat_fun_decl (env : string list Py_env.t)
      : Py_ast.file -> string list Py_env.t * Ast.parser_expr =
    (* inside function: return expected, otherwise give Unit *)
    function
    | [] -> env, (Ast.Const Ast.Unit |> dannot)
    | x::l ->
       match x with
       | Py_ast.Dimport _ ->
          Format.fprintf !wrn_fmt "Warning: import not supported yet\n%!";
          env, (treat_fun_decl env l |> snd)
       | Py_ast.Ddef _ -> failwith "TODO fun decl in fun"
       | Py_ast.Dstmt s when is_ret s -> (* return = ignore l *)
          env, treat_stmt env s
       | Py_ast.Dstmt s ->
          env, (Ast.Let ("_"
                       ,treat_stmt env s
                       ,treat_fun_decl env l |> snd
                 ) |> annot s.stmt_loc)
  in
  let treat_decl (env, l : string list Py_env.t * Ast.parser_program) =
    (* at toplevel *)
    function
    | Py_ast.Dimport _ ->
       Format.fprintf !wrn_fmt "Warning: import not supported yet\n%!";
       env, l
    | Py_ast.Ddef (fid, args, body) ->
       let env' = Py_env.add fid args env in
       let b = (treat_fun_decl env' body |> snd |> make_lambda args) in
       (env' (* allow recursion but ignore body's fun defs *)
       ,Ast.Definition (false, (fid, Ast.Lambda (Ast.Unnanoted, argn, b)
                                     |> dannot))
        ::l)
    | Py_ast.Dstmt s ->
       env, Ast.Definition (false, ("_", treat_stmt env s)) ::l
  in
  List.fold_left
    treat_decl
    (Py_env.empty, [])
    py_ast
  |> snd |> List.rev (* fold_right ? *)

let main f =
  try
    let py_ast : Py_ast.file =
      match f with
      | `Py_file fn -> parse_py_file fn
      | `Py_string str -> parse_py_string str
    in
    Printf.printf "File parsed: \n%s\n%!" Py_ast.(show_file py_ast);
    Printf.printf "File translated: \n%s\n%!" (py_to_source py_ast
                                             |> Ast.show_parser_program);
  with
  | FunArgs (pos, f, nb, giv) ->
     Format.fprintf !err_fmt "Error: Wrong number of arguments given to %s: \
                              expected %d arguments, given %d.\n  %s\n"
       f nb giv (Position.string_of_pos pos)
  | Undefined (pos, var) ->
     Format.fprintf !err_fmt "Error: Undefined variable %s.\n  %s\n%!" var
       (Position.string_of_pos pos)
  | e ->
     let msg = Printexc.to_string e
     and stack = Printexc.get_backtrace () in
     Format.fprintf !err_fmt "Uncaught exception: %s%s\n%!" msg stack
