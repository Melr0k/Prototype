open Parsing.IO
open System
open Types.Base
open Types.Additions
open Parsing
open Parsing.Variable
open Types.Tvar

type def = Variable.t * Ast.annot_expr * typ option

type typecheck_result =
  | TSuccess of typ * Env.t * (float * float)
  | TFailure of (Position.t list) * string * (float * float)

module Reconstruct = Reconstruction.Make ()

let generalize_all ~uncorrelate t =
  let aux = if uncorrelate then uncorrelate_tvars else Utils.identity in
  Subst.apply (generalize (vars t)) t |> aux |> bot_instance |> simplify_typ

exception IncompatibleType of typ
let type_check_def tenv env (var,expr,typ_annot) =
  let time0 = Unix.gettimeofday () in
  let (expr, addition) = Msc.remove_patterns_and_fixpoints expr in
  let nf_expr = Msc.convert_to_msc expr in
  let nf_addition =
    addition |> List.map (fun (v,e) -> v, Msc.convert_to_msc e) in
  let time1 = Unix.gettimeofday () in
  let retrieve_times () =
    let time2 = Unix.gettimeofday () in
    let msc_time = (time1 -. time0) *. 1000. in
    let typ_time = (time2 -. time1) *. 1000. in
    (msc_time, typ_time)
  in
  let type_additionnal env (v, nf) =
    let typ = Reconstruct.typeof_infer tenv env nf
              |> generalize_all ~uncorrelate:true in
    (* NOTE: ~uncorrelate:false can reduce the number of tvars in fixpoint
       instances, BUT it might also yield an unprecise type (expansion becomes
       necessary)... *)
    Env.add v typ env
  in
  try
    Utils.log "%a@." Msc.pp_e nf_expr ;
    let env = List.fold_left type_additionnal env nf_addition in
    Reconstruct.set_caching_status true ;
    let typ = Reconstruct.typeof_infer tenv env nf_expr
              |> generalize_all ~uncorrelate:true in
    (* Reconstruct.set_caching_status false ;
       let typ' = Reconstruct.typeof_infer tenv env nf_expr |> generalize_all in
       assert (subtype_poly typ typ' && subtype_poly typ' typ) ; *)
    let typ =
      match typ_annot with
      | None -> typ
      | Some typ' ->
         if subtype_poly typ typ'
         then typ' |> generalize_all ~uncorrelate:false
         else raise (IncompatibleType typ)
    in
    let env = Env.add var typ env in
    TSuccess (typ, env, retrieve_times ())
  with
  | Algorithmic.Untypeable (pos, str) ->
     TFailure (pos, str, retrieve_times ())
  | IncompatibleType _ ->
     TFailure (Variable.get_locations var,
               "the type inferred is not a subtype of the type specified",
               retrieve_times ())

type parsing_result =
  | PSuccess of type_env * (Ast.SE.t Ast.PureEnv.t) * ((int * def) list)
  | PFailure of Position.t * string

let builtin_functions =
  let open Variable in
  let arith_operators_typ, arith_unary_op_typ =
    let int = cons int_typ in
    let arith_u_op = mk_arrow int int in
     mk_arrow int (arith_u_op |> cons)
    ,arith_u_op
  in
  let open Ast.SE in
  let pure2 = Ast.SE.(cons pure pure1) in
  [ ("+"       , arith_operators_typ, pure2)
  ; ("-"       , arith_operators_typ, pure2)
  ; ("*"       , arith_operators_typ, pure2)
  ; ("/"       , arith_operators_typ, pure2)
  ; ("%"       , arith_operators_typ, pure2)
  ; ("succ"    , arith_unary_op_typ , pure1)
  ; (ref_create, fun_create_ref_typ , of_eff allocate)
  ; (ref_get   , fun_get_ref_typ    , of_eff read)
  ; (ref_set   , fun_set_ref_typ    , of_eff write)
  ]

let initial_varm =
  builtin_functions
  |> List.fold_left (fun varm (name, _, _) ->
         let var = Variable.create_other (Some name) in
         StrMap.add name var varm
       ) Ast.empty_name_var_map

let initial_env =
  builtin_functions
  |> List.fold_left (fun env (name, t, _) ->
         let var = StrMap.find name initial_varm in
         Env.add var t env
       ) Msc.initial_env

let initial_penv =
  let open Ast in
  builtin_functions
  |> List.fold_left
       (fun penv (name, _, se) -> PureEnv.add name se penv)
       PureEnv.empty

let parse_and_resolve f varm (penv:Ast.penv) =
  let last_pos = ref Position.dummy in
  try
    let ast =
      match f with
      | `File fn -> parse_program_file fn
      | `String s -> parse_program_string s
    in
    let treat_elem (tenv,varm,penv,defs) (annot, elem) =
      last_pos := Position.position annot ;
      match elem with
      | Ast.Definition (log, (name, expr, tyo)) ->
         let tyo = match tyo with
           | None -> None
           | Some expr -> let (t, _) = type_expr_to_typ tenv empty_vtenv expr in
                          Some t
         in
         let expr = Ast.parser_expr_to_annot_expr
                      tenv empty_vtenv varm penv expr in
         let var = Variable.create_other (Some name) in
         Variable.attach_location var (Position.position annot) ;
         let penv =
           Ast.(PureEnv.add name SE.(se_of expr |> tl |> cons pure) penv) in
         let varm = StrMap.add name var varm in
         (tenv,varm,penv,(log,(var,expr,tyo))::defs)
      | Ast.Atoms lst ->
         let tenv = List.fold_left define_atom tenv lst in
         let penv = List.fold_left (fun penv v ->
                        Ast.(PureEnv.add v SE.pure0 penv))
                      penv lst in
         (tenv,varm,penv,defs)
      | Ast.Types lst ->
         let (tenv, _) = define_types tenv empty_vtenv lst in
         (tenv,varm,penv,defs)
    in
    let (tenv, _, penv, defs) =
      List.fold_left treat_elem (empty_tenv, varm, penv, []) ast in
    PSuccess (tenv, penv, List.rev defs)
  with
  | Ast.LexicalError(pos, msg) -> PFailure (pos, msg)
  | Ast.SyntaxError (pos, msg) -> PFailure (pos, msg)
  | Ast.SymbolError msg -> PFailure (!last_pos, msg)
  | TypeDefinitionError msg -> PFailure (!last_pos, msg)
