open Main

let () =
  Printexc.record_backtrace true;
  Colors.add_ansi_marking Format.std_formatter;
  try
    let fn = ref "test.ml" in
    if Array.length Sys.argv > 1 then fn := Sys.argv.(1) ;
    match parse_and_resolve (`File !fn) initial_varm initial_penv with
    | PSuccess (tenv, lst) ->
       let time1 = Unix.gettimeofday () in
       List.fold_left (fun env (ll, (v, e, ta)) ->
           Utils.log_level := ll ;
           Format.printf "@{<green>%s@}: %!"
             (Parsing.Variable.Variable.get_name v |> Option.get) ;
           match type_check_def tenv env (v,e,ta) with
           | TSuccess (t, env, (tmsc, ttype)) ->
              Format.printf
                "%s (checked in %.02fms (msc:%.02fms, type:%.02fms), \
                 pure:@{<green>%s@}@{<red>%s@})\n%!"
                (Types.Tvar.string_of_type_short t)
                (tmsc +. ttype) tmsc ttype
                (if Parsing.Ast.is_pure e then "yes" else "")
                (if Parsing.Ast.is_pure e |> not then "no" else "");
              env
           | TFailure (pos, msg, _) ->
              Format.printf "Ill typed\n%!" ;
              Utils.error Format.std_formatter pos msg ;
              env
         ) initial_env lst |> ignore ;
       let time2 = Unix.gettimeofday () in
       Format.printf "@.@{<bold>Total time: %.02fs@}@." (time2 -. time1)
    | PFailure (pos, msg) ->
       Format.printf "@{<bold;red>Error at pos %s@}: %s\n%!"
         (Position.string_of_pos pos) msg
  with e ->
    let msg = Printexc.to_string e
    and stack = Printexc.get_backtrace () in
    Format.printf "@{<bold>@{<red>Uncaught exception@}: %s@}\n%s\n%!" msg stack
