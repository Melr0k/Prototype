open Common
open Source
open Typing

open IO
open Msc
open Types_additions
open Variable

let print_logs () =
  (*let treat (exprid, data)  =
    if data.visited = 0 && data.ignored > 0
    then Utils.warning data.position "Expression is unreachable!"
  in
  Seq.iter treat (all_logs ()) ;
  clear_logs ()*)
  ()

let print_ill_typed (pos, str) =
  Format.fprintf !std_fmt "%s\n%!" ("Ill typed" |> Utils.colorify Red) ;
  Utils.error pos str

let print_result str =
  Format.fprintf !std_fmt "%s@?" str

let type_check_program ~(from_py:bool)
      (program:Ast.parser_program) (pr:string -> unit) pr_logs pr_ill_typed =
  let test_def (tenv,varm,env,stenv,bvars) (name,parsed_expr) =
    Format.ksprintf pr "%s " (name ^ ":" |> Utils.colorify White) ;
    begin
      let var = Variable.create (Some name) in
      let ((_,v_st),_) as annot_expr =
        Ast.parser_expr_to_annot_expr ~from_py
          tenv empty_vtenv varm stenv parsed_expr in
      let time0 = Unix.gettimeofday () in
      let nf_expr = convert_to_msc ~legacy:true bvars annot_expr in
      let time1 = Unix.gettimeofday () in

      let fvelist = fv_e nf_expr in
      let envlist = Env.domain env |> VarSet.of_list in
      if not (VarSet.subset fvelist envlist)
      then begin
          Format.fprintf !err_fmt "traduction de l'ast:\n%s\n"
            (show_e nf_expr);
          failwith (Printf.sprintf "vars in fv_e, not in env: %s"
                      (VarSet.find_first (fun e -> VarSet.mem e envlist
                                                   |> not) fvelist
                       |> Variable.get_name
                       |> Option.get))
        end;
      let tmp_log = !Utils.log_enabled in
      Utils.log_enabled := false ;
      let typ_legacy =
        try Some (Old_checker.typeof_simple_legacy tenv env nf_expr)
        with Old_checker.Ill_typed _ -> None
      in
      Utils.log_enabled := tmp_log ;
      try
        (*Format.printf "%a@." pp_e nf_expr ;*)
        let typ = Checker.typeof_simple tenv env nf_expr in
        let time2 = Unix.gettimeofday () in

        let msc_time = (time1 -. time0) *. 1000.
        and typ_time = (time2 -. time1) *. 1000.
        and time = (time2 -. time0) *. 1000. in

        let varm = StrMap.add name var varm in
        let env = Env.add var typ env in
        let stenv = VarMap.add var v_st stenv
        in

        Format.ksprintf pr
          "%s (checked in %.02fms (msc:%.02fms, type:%.02fms))\n"
          (Cduce.string_of_type typ) time msc_time typ_time;
        begin match typ_legacy with
        | None -> Format.ksprintf pr "%s"
                    ("===== Good news: Was untypable with POPL22 system =====\n"
                     |> Utils.colorify Green)
        | Some t ->
          if Cduce.subtype typ t |> not
          then (
            Format.ksprintf pr "%s %s\n"
              ("===== Warning: Not better than the type obtained by POPL22 \
                system =====\nType was:"
               |> Utils.colorify Yellow)
              (Cduce.string_of_type t)
          (*; Format.printf "%a@." pp_e nf_expr*)
          )
        end ;
        pr_logs () ; (varm, env, stenv)
      with Checker.Ill_typed (pos, str) ->
        (*Format.printf "%a@." pp_e nf_expr ;*)
        pr_ill_typed (pos, str);
        begin match typ_legacy with
        | None -> ()
        | Some t ->
           Format.ksprintf pr "%s %s\n"
             ("===== Warning: Was typable with POPL22 system =====\nType was:"
              |> Utils.colorify Yellow)
             (Cduce.string_of_type t);
           pr_logs ()
        end ;
        (varm,env,stenv)
    end
  in
  let treat_elem (tenv,varm,env,stenv,bvars) elem =
    match elem with
    | Ast.Definition (log, d) ->
       if log then Utils.log_enabled := true ;
       let (varm,env,stenv) = test_def (tenv,varm,env,stenv,bvars) d in
       Utils.log_enabled := false ;
       (tenv,varm,env,stenv,bvars)
    | Ast.Atoms lst ->
       let tenv = List.fold_left define_atom tenv lst in
       (tenv,varm,env,stenv,bvars)
    | Ast.Types lst ->
       let (tenv, _) = define_types tenv empty_vtenv lst in
       (tenv,varm,env,stenv,bvars)
  in
  let ini_name_var_map, ini_env, st_env, builtin_vars =
    List.fold_left
      (fun (varm, env, st, bv) (name, typ, se) ->
        let var = Variable.create (Some name) in
        ( StrMap.add name var varm
        , Env.add var typ env
        , VarMap.add var se st
        , (name, var)::bv )
      )
      (Ast.empty_name_var_map, Env.empty, VarMap.empty, [])
      (*  name      , type : typ          , side-effects  *)
      [ ( ref_create, Cduce.fun_create_ref, Ast.no_se    )
      ; ( ref_get   , Cduce.fun_get_ref   , Ast.read_se  )
      ; ( ref_set   , Cduce.fun_set_ref   , Ast.write_se ) ]
  in
  ignore (List.fold_left
            treat_elem
            (empty_tenv, ini_name_var_map, ini_env, st_env, builtin_vars)
            program)

let main f =
  Printexc.record_backtrace true;
  try
    let (ast : Ast.parser_program), from_py =
      match f with
      | `File fn -> parse_program_file fn, false
      | `String s -> parse_program_string s, false
      | `Python p -> Py_to_source.translate_input p, true
    in
    Printf.printf "%s\n%s\n%!"
      ("got an ast:" |> Utils.colorify Green)
      (Ast.show_parser_program ast);
    type_check_program ~from_py ast print_result print_logs print_ill_typed
  with
  (* Source *)
  | Ast.LexicalError(pos, msg) ->
     Format.fprintf !err_fmt "Lexical error at position %d, %s\n%!" pos msg
  | Ast.SyntaxError (spos, msg) ->
     Format.fprintf !err_fmt "%s, %s\n%!" spos msg
  | Ast.UndefinedSymbol s ->
     Format.fprintf  !err_fmt "Error: undefined symbol `%s'\n%!" s
  (* Python *)
  | Py_to_source.SyntaxError (pos, msg) ->
     Format.fprintf !err_fmt "%s\nSyntax error: %s\n"
       (Position.string_of_pos pos) msg
  | Py_to_source.Undefined (pos, var) ->
     Format.fprintf !err_fmt "%s\nName error: Undefined variable %s.\n%!"
       (Position.string_of_pos pos) var
  (* other *)
  | e ->
     let msg = Printexc.to_string e
     and stack = Printexc.get_backtrace () in
     Format.fprintf !err_fmt "Uncaught exception: %s%s\n%!" msg stack
