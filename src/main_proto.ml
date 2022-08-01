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

let type_check_program
      (program:Ast.parser_program) (pr:string -> unit) pr_logs pr_ill_typed =
  let test_def (tenv,varm,env) (name,parsed_expr) =
    Format.ksprintf pr "%s " (name ^ ":" |> Utils.colorify_l [Bold]) ;
    begin
      let var = Variable.create (Some name) in
      let annot_expr = Ast.parser_expr_to_annot_expr
                         tenv empty_vtenv varm parsed_expr in
      let time0 = Unix.gettimeofday () in
      let nf_expr = convert_to_msc ~legacy:true annot_expr in
      let time1 = Unix.gettimeofday () in
      assert (VarSet.subset (fv_e nf_expr) (Env.domain env |> VarSet.of_list)) ;
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
        pr_logs () ; (varm, env)
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
        (varm,env)
    end
  in
  let treat_elem (tenv,varm,env) elem =
    match elem with
    | Ast.Definition (log, d) ->
       if log then Utils.log_enabled := true ;
       let (varm,env) = test_def (tenv,varm,env) d in
       Utils.log_enabled := false ;
       (tenv,varm,env)
    | Ast.Atoms lst ->
       let tenv = List.fold_left define_atom tenv lst in
       (tenv,varm,env)
    | Ast.Types lst ->
       let (tenv, _) = define_types tenv empty_vtenv lst in
       (tenv,varm,env)
  in
  ignore (List.fold_left
            treat_elem
            (empty_tenv, Ast.empty_name_var_map, Env.empty)
            program)

let main f =
  Printexc.record_backtrace true;
  try
    let ast : Ast.parser_program =
      match f with
      | `File fn -> parse_program_file fn
      | `String s -> parse_program_string s
    in
    type_check_program ast print_result print_logs print_ill_typed
  with
  | Ast.LexicalError(pos, msg) ->
     Format.fprintf !err_fmt "Lexical error at position %d, %s\n%!" pos msg
  | Ast.SyntaxError (spos, msg) ->
     Format.fprintf !err_fmt "%s, %s\n%!" spos msg
  | Ast.UndefinedSymbol s ->
     Format.fprintf  !err_fmt "Error: undefined symbol `%s'\n%!" s
  | e ->
     let msg = Printexc.to_string e
     and stack = Printexc.get_backtrace () in
     Format.fprintf !err_fmt "Uncaught exception: %s%s\n%!" msg stack
