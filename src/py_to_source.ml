open Common
open Source
open Python

open IO

exception SyntaxError of Position.t * string (* pos, msg *)
exception Undefined of Position.t * string (* pos, var *)

let fun_args_error f nb giv =
  Printf.sprintf "Wrong number of arguments given to %s: \
                  expected %d arguments, given %d." f nb giv

let translate py_ast =
  let module Py_env = Map.Make(String) in
  (* functions → their arguments
     or variables → is it a ref ? *)

  let annot p ast = Ast.new_annot p, ast in
  let dannot ast = annot Position.dummy ast in (* semi-dummy annotation *)

  let argn = "__args__" in (* argument of all function *)
  let argvar = Ast.Var argn |> dannot in

  (* fan v l = forward analysis :
     Is v assigned to a value in l ? Should v be a ref ?

     In case of a call, what should we do?
     => x = 5
        g(x)  <- perhaps x needs to be a ref, but we havn't run typechecking yet
   *)
  let fan ?(allow=false) v l =
    (* allow = give true on the second assign found: allow 1 *)
    let rec aux allow =
      let find_v_opt = List.find_opt (fun e -> e = v) in
      function
      | [] -> false
      | s::l ->
         match s with (* 🤔 can we use a monad here ? *)
         | Py_ast.Dimport (_, idl) ->
            begin match find_v_opt idl with
            | Some _ -> false (* override *)
            | None -> aux allow l
            end
         | Py_ast.Ddef (fid, _, _) -> (* we don't have the `global` directive *)
            if fid = v then false else aux allow l
         | Py_ast.Dstmt s ->
            match s.stmt_desc with
            | Py_ast.Sassign (id, _) when id = v ->
               not allow || aux false l
            | Py_ast.Sif (_, e1, e2) ->
               aux allow e1 || aux allow e2 || aux allow l
            | Py_ast.Sfor (_, _, b) | Py_ast.Swhile (_, b) ->
                (* useless, not treated by treat_* *)
               aux allow b || aux allow l
            | _ -> aux allow l (* Scall case left... *)
    in aux allow l
  in

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
         Ast.RecordUpdate (aux (n+1) (xl, al), x, Some a) |> annot loc
      | _, [] | [], _ ->
         SyntaxError (loc, fun_args_error f
                             (List.length varl)
                             (List.length astl)) |> raise
    in
    aux 0 (varl, astl)
  in
  let upd_env (venv, fenv) (args, body) =
    (* variable's scope in python is weird, but it's ok *)
    let rec get_def = function
      | Py_ast.Dimport (_, idl) -> idl
      | Py_ast.Ddef (fid, _, _) -> [fid]
      | Py_ast.Dstmt s ->
         match s.stmt_desc with
         | Py_ast.Sassign (id, _) -> [id]
         | _ -> [] (* ignore definitions in if blocks: it's another scope *)
    and fold_def = fun acc s -> get_def s @ acc in
    let venv, vassign =
      List.( fold_left (fun acc x -> Py_env.add x false acc) venv args
           , fold_left fold_def [] body)
    in
    (* for all v in vassign, assoc with fan value & return env *)
    ( List.fold_left
        (fun acc v -> Py_env.add v (fan ~allow:true v body) acc)
        venv
        vassign
    , fenv )
  in

  (* /!\ Theses functions are not defined in source language /!\ *)
  let binop b =
    Ast.Var (match b with
             | Py_ast.Badd -> "+"
             | Py_ast.Bsub -> "-"
             | Py_ast.Bmul -> "*"
             | Py_ast.Bdiv -> "/"
             | Py_ast.Bmod -> "%"
             | Py_ast.Beq  -> "="
             | Py_ast.Bneq -> "<>"
             | Py_ast.Blt  -> "<"
             | Py_ast.Ble  -> "<="
             | Py_ast.Bgt  -> ">"
             | Py_ast.Bge  -> ">="
             | Py_ast.Band -> "&&"
             | Py_ast.Bor  -> "||"
      ) |> dannot
  and unop u =
    Ast.Var (match u with
             | Py_ast.Uneg -> "-"
             | Py_ast.Unot -> "!"
      ) |> dannot
  in
  let rec treat_expr (venv, fenv as env) (e:Py_ast.expr) =
    let aux = function
      | Py_ast.Enone -> Ast.(Const Unit)
      | Py_ast.Ebool b -> Ast.(Const (Bool b))
      | Py_ast.Eint istr -> Ast.(Const (Int (int_of_string istr)))
      | Py_ast.Estring str -> Ast.(Const (String str))
      | Py_ast.Eident id ->
         if Py_env.find_opt id venv = Some true
         then Ast.(Read (Var id |> annot e.expr_loc))
         else Ast.(Var id)
      | Py_ast.Ebinop (b, e1, e2) ->
         Ast.(App ( App (binop b, treat_expr env e1) |> annot e.expr_loc
                  , treat_expr env e2)
         )
      | Py_ast.Eunop (u, e) ->
         Ast.(App (unop u, treat_expr env e))
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
      | Py_ast.Elist _ -> failwith "Lists not supported yet."
      | Py_ast.Emake _ -> failwith "Make list not supported yet."
      | Py_ast.Eget _ -> failwith "Get not supported yet."
    in aux e.expr_desc |> annot e.expr_loc
  in
  let treat_stmt_if_test env t =
    let open Py_ast in
    match t.expr_desc with
    (* if (type(...) == ...): ...  =>  if ... is ... then ... *)
    | Ebinop ( Beq
             , {expr_desc=Ecall ("type", [e0]); expr_loc=_}
             , {expr_desc=Estring typ; expr_loc=_} )
      | Ebinop ( Beq
               , {expr_desc=Ecall ("type", [e0]); expr_loc=_}
               , {expr_desc=Eident typ; expr_loc=_} )->
       ( treat_expr env e0
       , (match List.assoc_opt typ basic_types with
            Some t -> t | _ -> TCustom typ) )
    | _ -> ( treat_expr env t
           , TBase TTrue )
  in
  let rec treat_stmt ?(topl=false) env (s:Py_ast.stmt) =
    let aux = function
      | Py_ast.Sif _ | Py_ast.Sassign _ ->
         assert false (* should be treated in upper levels in the AST *)
      | Py_ast.Sreturn _ when topl ->
         SyntaxError (s.stmt_loc, "Return outside function") |> raise
      | Py_ast.Sreturn e | Py_ast.Seval e ->
         treat_expr env e |> snd
      | Py_ast.Swhile _ -> failwith "While not supported yet."
      | Py_ast.Sfor _ -> failwith "For not supported yet."
      | Py_ast.Sset _ -> failwith "Set not supported yet."
      | Py_ast.Sbreak -> failwith "Break not supported yet."
    in aux s.stmt_desc |> annot s.stmt_loc
  and treat_decl_ddef (venv, fenv) (fid, args, body) = (* Py_ast.Ddef *)
    let venv',fenv' = Py_env.(add fid false venv, add fid args fenv) in
    let b = (treat_uni_decl
               ( List.fold_left
                   (fun ve e ->             (* We suppose *)
                     Py_env.add e true ve)  (* args to be *)
                   venv'                    (* ref vars ? *)
                   args
               , fenv' ) body
             |> make_lambda args)
    in (venv', fenv'), b
  and treat_uni_decl ?(topl=false) (venv, fenv as env)
      : Py_ast.file -> Ast.parser_expr =
    (* gives back an ast (not a list of ast) *)
    let aux next = function
      | Py_ast.Dimport _ ->
         Format.fprintf !wrn_fmt "Warning: import not supported yet.\n%!";
         `Pass
      | Py_ast.Ddef (fid, args, body) ->
         let env, b =
           treat_decl_ddef (upd_env env (args,body)) (fid, args, body) in
         `Let (env, fid, b, dannot)
      | Py_ast.Dstmt s ->
         begin match s.stmt_desc with
         | Py_ast.Sif (t, e1, e2) ->
            let (e0, t) = treat_stmt_if_test env t in
            `Instr ( env
                   , Ast.Ite ( e0, t
                               , treat_uni_decl ~topl env e1
                               , treat_uni_decl ~topl env e2 )
                     |> annot s.stmt_loc )
         | Py_ast.Sreturn _ ->
            `Val (treat_stmt ~topl env s) (* return = ignore l *)
         | Py_ast.Sassign (v, e) ->
            if Py_env.mem v venv (* v ∈ Γ *)
            then if Py_env.find v venv (* v : Ref *)
                 then `Instr ( env
                             , Ast.(Assign ( Var v |> dannot
                                           , treat_expr env e))
                               |> annot s.stmt_loc )
                 else begin
                     (* * )
                       Format.fprintf !wrn_fmt
                       "%s defined, not ref, but assigned. Should occur \
                        only once, and python probably don't accept it.\n%!" v;
                     ( * *)
                     let v_is_ref = fan v next in
                     let e1 = treat_expr env e
                     in `Let (env, v, (if v_is_ref
                                       then Ast.Ref e1 |> dannot
                                       else e1), annot s.stmt_loc)
                   end
            else
              let v_is_ref = fan v next in
              let e1 = treat_expr env e
              in `Let ( (Py_env.add v v_is_ref venv, fenv)
                      , v
                      , (if v_is_ref
                         then Ast.Ref e1 |> dannot
                         else e1)
                      , annot s.stmt_loc )
         | _ ->
            `Instr ( env
                   , treat_stmt ~topl env s )
         end
    in
    function
    | [] -> (Ast.Const Ast.Unit |> dannot)
    | x::l ->
       match aux l x with
       | `Pass -> treat_uni_decl ~topl env l
       | `Let (env, v, e, fann) ->
          Ast.Let (v, e, treat_uni_decl ~topl env l) |> fann
       | `Instr (env, e) ->
          if l=[] && topl
          then e
          else Ast.Let ("_", e, treat_uni_decl ~topl env l) |> dannot
       | `Val v -> v
  and treat_decl (venv, fenv as env : (bool Py_env.t * string list Py_env.t))
      : Py_ast.file -> Ast.parser_program
    = function (* at toplevel *)
    | [] -> []
    | decl::l ->
       match decl with
       | Py_ast.Dimport _ ->
          Format.fprintf !wrn_fmt "Warning: import not supported yet\n%!";
          treat_decl env l
       | Py_ast.Ddef (fid, args, body) ->
          let env', b = treat_decl_ddef env (fid, args, body) in
          Ast.Definition (false, (fid, Ast.Lambda (Ast.Unnanoted, argn, b)
                                       |> dannot))
          :: treat_decl env' l
       | Py_ast.Dstmt s ->
          begin match s.stmt_desc with
          | Py_ast.Sif (t, e1, e2) ->
             let (e0, t) = treat_stmt_if_test env t in
             Ast.Definition
               (false, ( "_"
                       , Ast.Ite ( e0, t
                                  , treat_uni_decl ~topl:true env e1
                                  , treat_uni_decl ~topl:true env e2 )
                         |> annot s.stmt_loc))
             :: treat_decl env l
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
          | _ -> Ast.Definition (false, ("_", treat_stmt ~topl:true env s))
                 :: treat_decl env l
          end
  in
  (* motivations for not unifying treat_uni_decl and treat_decl with some
     toplevel:bool, same for stmt cases managed in theses two firsts functions:

     1. toplevel imply differents types (and constructors) for decl functions:
        - treat_uni_decl : 'env -> Py_ast.file -> Ast.parser_expr
        - treat_decl : 'env -> Py_ast.file -> Ast.parser_program
     2. the variable used in Ast.Definition depends on the stmt (assign or not),
        if treat_stmt takes care of it it should have a different return type
        only for 1 case… and it needs the next expressions for the forward
        analysis, so 1 more argument only for one case. Same for return. *)
  treat_decl
    Py_env.(empty, empty)
    py_ast

let translate_input p =
  let py_ast : Py_ast.file =
    match p with
    | `File fn -> parse_py_file fn
    | `String str -> parse_py_string str
  in
  translate py_ast
