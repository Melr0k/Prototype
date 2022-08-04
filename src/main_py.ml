open Common
open Source
open Python

open IO

exception SyntaxError of Position.t * string (* pos, msg *)
exception Undefined of Position.t * string (* pos, var *)

let fun_args_error f nb giv =
  Printf.sprintf "Wrong number of arguments given to %s: \
                  expected %d arguments, given %d." f nb giv

let py_to_source py_ast =
  let module Py_env = Map.Make(String) in
  (* functions → their arguments
     or variables → is it a ref ? *)

  let annot p ast = Ast.new_annot p, ast in
  let dannot ast = annot Position.dummy ast in (* semi-dummy annotation *)

  let argn = "__args__" in (* argument of all function *)
  let argvar = Ast.Var argn |> dannot in

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
       Ast.(Let ( x
                , Projection (Field x, argvar) |> dannot
                , body')) |> dannot
  in
  let make_app f loc varl astl =
    let rec aux n = function
      | []   , []    -> Ast.(Const EmptyRecord) |> annot loc
      | x::xl, a::al ->
         Ast.RecordUpdate (a, x, Some (aux (n+1) (xl, al))) |> annot loc
      | _, [] ->
         SyntaxError (loc, fun_args_error f (List.length varl + n) n) |> raise
      | [], _ ->
         SyntaxError (loc, fun_args_error f n (List.length astl + n)) |> raise
    in
    aux 0 (varl, astl)
  in

  (* fan v l = forward analysis :
     Is v assigned to a value in l ? Should v be a ref ?

     In case of a call, what should we do?
     => x = 5
        g(x)  <- perhaps x needs to be a ref, but we havn't run typechecking yet
   *)
  let rec fan v =
    let find_v_opt = List.find_opt (fun e -> e = v) in
    function
    | [] -> false
    | s::l ->
       match s with (* 🤔 can we use a monad here ? *)
       | Py_ast.Dimport (_, idl) ->
          begin match find_v_opt idl with
          | Some _ -> false
          | None -> fan v l
          end
       | Py_ast.Ddef (fid, args, body) ->
          if fid = v
          then false
          else
            begin match find_v_opt args with
            | Some _ -> fan v l (* overwritten inside the body *)
            | None -> (fan v body || fan v l)
            end
       | Py_ast.Dstmt s ->
          match s.stmt_desc with
          | (Py_ast.Sassign (id, _)) when id = v -> true
          | _ -> fan v l (* Scall case left... *)
  in

  let rec treat_expr (venv, fenv) (e:Py_ast.expr) =
    let aux = function
      | Py_ast.Enone -> Ast.(Const Unit)
      | Py_ast.Eint str ->
         Ast.(Const (Int (int_of_string str)))
      | Py_ast.Eident id ->
         if Py_env.find_opt id venv = Some true
         then Ast.(Read (Var id |> annot e.expr_loc))
         else Ast.(Var id)
      | Py_ast.Ecall (fid, args) ->
         let argvars = match Py_env.find_opt fid fenv with
           | Some f -> f
           | None -> raise (Undefined (e.expr_loc,fid))
         in
         Ast.(App ( Var fid |> annot e.expr_loc
                  , List.fold_left
                      (fun acc e -> treat_expr (venv, fenv) e ::acc)
                      []
                      args
                    |> List.rev
                    |> make_app fid e.expr_loc argvars)
         )
      | _ -> failwith "TODO expr"
    in aux e.expr_desc |> annot e.expr_loc
  in
  let rec treat_stmt (env) (s:Py_ast.stmt) =
    let aux = function
      | Py_ast.Sif (t, e1, e2) ->
         let open Py_ast in
         begin match t.expr_desc with
         (* if (type(...) == ...): ...  =>  if ... is ... then ... *)
         | Ebinop ( Beq
                  , {expr_desc=Ecall ("type", [e0]); expr_loc=_}
                  , {expr_desc=Estring typ; expr_loc=_}) ->
            Ast.Ite ( treat_expr env e0
                    , (match List.assoc_opt typ basic_types with
                         Some t -> t | _ -> TCustom typ)
                    , treat_fun_decl env e1
                    , treat_fun_decl env e2 )
         | _ -> failwith "TODO normal if"
         end
      | Py_ast.Sreturn e | Py_ast.Seval e ->
         treat_expr env e |> snd
      | Py_ast.Sassign _ -> assert false (* treated in treat_fun_decl *)
      | _ -> failwith "TODO stmt"
    in aux s.stmt_desc |> annot s.stmt_loc
  and treat_fun_decl ((venv, fenv) as env : 'b Py_env.t * 'sl Py_env.t)
      : Py_ast.file -> Ast.parser_expr =
    (* inside function: return expected, otherwise give Unit *)
    function
    | [] -> (Ast.Const Ast.Unit |> dannot)
    | x::l ->
       begin match x with
       | Py_ast.Dimport _ ->
          Format.fprintf !wrn_fmt "Warning: import not supported yet\n%!";
          treat_fun_decl env l
       | Py_ast.Ddef _ -> failwith "TODO fun decl in fun"
       | Py_ast.Dstmt s ->
          begin match s.stmt_desc with
          | Py_ast.Sreturn _ -> treat_stmt env s (* return = ignore l *)
          | Py_ast.Sassign (v, e) ->
             let (v, e1, e2) =
               if Py_env.mem v venv (* v ∈ Γ *)
               then if Py_env.find v venv (* v : Ref *)
                    then "_"
                       , Ast.(Assign ( Var v |> dannot
                                     , treat_expr env e)) |> annot s.stmt_loc
                       , treat_fun_decl env l
                    else assert false (* x defined, not ref, but assigned ! *)
               else
                 let v_is_ref = fan v l in
                 let e1 = treat_expr env e
                 in v
                  , (if v_is_ref
                     then Ast.Ref e1 |> dannot
                     else e1)
                  , treat_fun_decl (Py_env.add v (fan v l) venv, fenv) l
             in
             Ast.Let (v, e1, e2) |> annot s.stmt_loc
          | _ ->
             (Ast.Let ( "_"
                      , treat_stmt env s
                      , treat_fun_decl env l
                ) |> annot s.stmt_loc)
          end
       end
  and treat_decl ((venv, fenv) as env : (bool Py_env.t * string list Py_env.t))
      : Py_ast.file -> Ast.parser_program
    = function (* at toplevel *)
    | [] -> []
    | decl::l ->
       match decl with
       | Py_ast.Dimport _ ->
          Format.fprintf !wrn_fmt "Warning: import not supported yet\n%!";
          treat_decl env l
       | Py_ast.Ddef (fid, args, body) ->
          let env' = Py_env.(add fid false venv, add fid args fenv) in
          let b = (treat_fun_decl env' body |> make_lambda args) in
          (* allow recursion but ignore body's fun defs *)
          Ast.Definition
            (false, (fid, Ast.Lambda (Ast.Unnanoted, argn, b) |> dannot))
          :: treat_decl env' l
       | Py_ast.Dstmt s ->
          begin match s.stmt_desc with
          | Py_ast.Sreturn _ ->
             SyntaxError (s.stmt_loc, "Return outside function") |> raise
          | Py_ast.Sassign (v,e) ->
             let (venv, v, e) =
               if Py_env.mem v venv (* v ∈ Γ *)
               then if Py_env.find v venv (* v : Ref *)
                    then venv
                       , "_"
                       , Ast.(Assign ( Var v |> dannot
                                     , treat_expr env e))
                    else assert false (* x defined, not ref, but assigned ! *)
               else
                 let v_is_ref = fan v l in
                 let venv = Py_env.add v v_is_ref venv in
                 let e = treat_expr env e
                 in venv
                  , v
                  , if v_is_ref
                    then Ast.Ref e
                    else snd e
             in
             Ast.Definition (false, (v, annot s.stmt_loc e))
             :: treat_decl (venv,fenv) l
          | _ -> Ast.Definition (false, ("_", treat_stmt env s))
                 :: treat_decl env l
          end
  in
  treat_decl
    Py_env.(empty, empty)
    py_ast

let main f =
  try
    let py_ast : Py_ast.file =
      match f with
      | `Py_file fn -> parse_py_file fn
      | `Py_string str -> parse_py_string str
    in
    Printf.printf "%s\n%s\n%!" ("File parsed:" |> Utils.colorify Green)
      Py_ast.(show_file py_ast);
    Printf.printf "\n%s\n%s\n%!" ("File translated:" |> Utils.colorify Green)
      (py_to_source py_ast |> Ast.show_parser_program);
  with
  | SyntaxError (pos, msg) ->
     Format.fprintf !err_fmt "%s\nSyntax error: %s\n"
       (Position.string_of_pos pos) msg
  | Undefined (pos, var) ->
     Format.fprintf !err_fmt "%s\nName error: Undefined variable %s.\n%!"
       (Position.string_of_pos pos) var
  | e ->
     let msg = Printexc.to_string e
     and stack = Printexc.get_backtrace () in
     Format.fprintf !err_fmt "Uncaught exception: %s%s\n%!" msg stack
