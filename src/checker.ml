open Cduce
open Msc
open Types_additions
open Annotations
open Variable
open Utils
(* TODO: Improve error messages
   (when failing due to all branches having failed, print errors of the branches) *)
(* TODO: Better inference of the domain of functionnal arguments
   (and then, support for recursive functions) *)

exception Ill_typed of Position.t list * string

let splits_domain splits domain =
  Format.asprintf "Splits: %a - Domain: %a"
    (Utils.pp_list Cduce.pp_typ) splits Cduce.pp_typ domain

let actual_expected act exp =
  Format.asprintf "Actual: %a - Expected: %a" pp_typ act pp_typ exp

let unbound_variable pos v =
  raise (Ill_typed (pos, "Unbound variable "^(Variable.show v)^"."))

let var_type pos v env =
  if Env.mem v env then Env.find v env else unbound_variable pos v

let typeof_const_atom tenv c =
  match c with
  | Ast.Atom str -> get_type tenv str
  | c -> Ast.const_to_typ c

let rec typeof_a pos tenv env a =
  let type_lambda env va v e =
    let splits = VarAnnot.splits env va in
    if splits = []
    then raise (Ill_typed (pos, "Cannot infer domain of this abstraction."))
    else begin
      splits |> List.map (fun t ->
        let env = Env.add v t env in
        let res = typeof tenv env e in
        mk_arrow (cons t) (cons res)
      ) |> conj |> simplify_typ
    end
  in
  match a with
  | Abstract t -> t
  | Const c -> typeof_const_atom tenv c
  | Pair (v1, v2) ->
    let t1 = var_type pos v1 env in
    let t2 = var_type pos v2 env in
    mk_times (cons t1) (cons t2)
  | Projection (Field label, v) -> 
    let t = var_type pos v env in
    if subtype t record_any then
      try get_field t label
      with Not_found -> raise (Ill_typed (pos, "Label " ^ label ^ " not present."))
    else
      raise (Ill_typed (pos, "Field projection can only be done on a record."))
  | Projection (p, v) ->
    let t = var_type pos v env in
    if subtype t pair_any
    then (if p = Fst then pi1 t else pi2 t)
    else raise (Ill_typed (pos, "Projection can only be done on a pair."))
  | RecordUpdate (r, label, None) -> 
    let t = var_type pos r env in
    if subtype t record_any then
      remove_field t label
    else
      raise (Ill_typed (pos, "Field removal can only be done on a record."))
  | RecordUpdate (r, label, Some v) ->
    let t = var_type pos r env in
    let t' = var_type pos v env in
    if subtype t record_any then
      let right_record = mk_record false [label, cons t'] in
      merge_records t right_record
    else
      raise (Ill_typed (pos, "Field update can only be done on a record."))
  | App (v1, v2) ->
    let t1 = var_type pos v1 env in
    if subtype t1 arrow_any
    then
      let t2 = var_type pos v2 env in
      let dom = domain t1 in
      if subtype t2 dom
      then apply t1 t2
      else raise (Ill_typed (pos,
        "Argument not in the domain of the function. "^(actual_expected t2 dom)))
    else raise (Ill_typed (pos, "Application can only be done on a function."))
  | Ite (v, t, x1, x2) ->
    let tv = var_type pos v env in
    if subtype tv empty
    then empty
    else if subtype tv t
    then var_type pos x1 env
    else if subtype tv (neg t)
    then var_type pos x2 env
    else raise (Ill_typed (pos, "Cannot select a branch for the typecase."))
  | Lambda (va, Ast.ADomain s, v, e) ->
    let inferred_t = type_lambda env va v e in
    let dom = domain inferred_t in
    if equiv s dom
    then inferred_t
    else raise (Ill_typed (pos,
      "The inferred domain for the abstraction is different. "^(actual_expected dom s)))
  | Lambda (va, Ast.AArrow t, v, e) ->
    let inferred_t = type_lambda env va v e in
    if subtype inferred_t t
    then t
    else raise (Ill_typed (pos,
      "The inferred type for the abstraction is too weak. "^(actual_expected inferred_t t)))
  | Lambda (va, Unnanoted, v, e) -> type_lambda env va v e
  | Let (v1, v2) ->
    if Env.mem v1 env
    then var_type pos v2 env
    else raise (Ill_typed (pos, "Unable to type the definition."))

and typeof tenv env e =
  match e with
  | Var v -> var_type (Variable.get_locations v) v env
  | Bind (va, v, a, e) ->
    let splits = VarAnnot.splits env va in
    if splits = []
    then typeof tenv env e
    else begin
      let pos = Variable.get_locations v in
      let s = typeof_a pos tenv env a in
      if disj splits |> equiv s
      (* NOTE: it is still sound if we only test 'subtype' instad of 'equiv',
        but for now I prefer to match the paper. *)
      then
        splits |> List.map (fun t ->
          let env = Env.add v t env in
          typeof tenv env e
        ) |> disj |> simplify_typ
      else raise (Ill_typed (pos,
        "Invalid splits (does not cover the initial domain). "^(splits_domain splits s)))
    end

let refine_a tenv env a t =
  match a with
  | Abstract s -> if disjoint s t then [] else [env]
  | Const c -> if disjoint (typeof_const_atom tenv c) t then [] else [env]
  | Pair (v1, v2) ->
    split_pair t
    |> List.filter_map (
      fun (t1, t2) ->
        env |>
        option_chain [Env_refinement.refine_existing v1 t1 ; Env_refinement.refine_existing v2 t2]
    )
  | Projection (Fst, v) -> [Env_refinement.refine_existing v (mk_times (cons t) any_node) env] |> filter_options
  | Projection (Snd, v) -> [Env_refinement.refine_existing v (mk_times any_node (cons t)) env] |> filter_options
  | Projection (Field label, v) ->
    [Env_refinement.refine_existing v (mk_record true [label, cons t]) env] |> filter_options
  | RecordUpdate (v, label, None) ->
    let t = cap_o t (record_any_without label) in
    split_record t
    |> List.filter_map (
      fun ti ->
          let ti = remove_field_info ti label in
          Env_refinement.refine_existing v ti env
    )
  | RecordUpdate (v, label, Some x) ->
    split_record t
    |> List.filter_map (
      fun ti ->
        let field_type = get_field_assuming_not_absent ti label in
        let ti = remove_field_info ti label in
        env |>
        option_chain [Env_refinement.refine_existing v ti ; Env_refinement.refine_existing x field_type]
      )
  | App (v1, v2) ->
    let t1 = Env_refinement.find v1 env in
    square_split t1 t
    |> List.filter_map (
      fun (t1, t2) ->
        env |>
        option_chain [Env_refinement.refine_existing v1 t1 ; Env_refinement.refine_existing v2 t2]
    )
  | Ite (v, s, x1, x2) ->
    [ env |> option_chain [Env_refinement.refine_existing v s       ; Env_refinement.refine_existing x1 t] ;
      env |> option_chain [Env_refinement.refine_existing v (neg s) ; Env_refinement.refine_existing x2 t] ]
    |> filter_options
  | Lambda _ ->
    if disjoint arrow_any t then [] else [env]
  | Let (v1, v2) ->
    [ env |>
    option_chain [Env_refinement.refine_existing v1 any ; Env_refinement.refine_existing v2 t]]
    |> filter_options

let propagate tenv x a gammas =
  gammas |>
  List.map (fun gamma ->
    if Env_refinement.has_refinement x gamma
    then refine_a tenv gamma a (Env_refinement.find x gamma)
    else [gamma]
  ) |> List.flatten

let empty_annots_a =
  map_a
  (function Bind (_, v, a, e) -> Bind (VarAnnot.empty, v, a, e) | e -> e)
  (function Lambda (_, t, v, e) -> Lambda (VarAnnot.empty, t, v, e) | a -> a)

let empty_annots_e =
  map_e
  (function Bind (_, v, a, e) -> Bind (VarAnnot.empty, v, a, e) | e -> e)
  (function Lambda (_, t, v, e) -> Lambda (VarAnnot.empty, t, v, e) | a -> a)

let restrict_annots_a gamma =
  map_a
  (function Bind (va, v, a, e) -> Bind (VarAnnot.restrict gamma va, v, a, e) | e -> e)
  (function Lambda (va, t, v, e) -> Lambda (VarAnnot.restrict gamma va, t, v, e) | a -> a)  

let restrict_annots_e gamma =
  map_e
  (function Bind (va, v, a, e) -> Bind (VarAnnot.restrict gamma va, v, a, e) | e -> e)
  (function Lambda (va, t, v, e) -> Lambda (VarAnnot.restrict gamma va, t, v, e) | a -> a)

let extract x gammas =
  let vas =
    gammas |> List.filter_map (fun envr ->
      if Env_refinement.mem x envr
      then
        Some (VarAnnot.singleton
          (Env_refinement.rm x envr |> Env_refinement.to_env)
          (Env_refinement.find x envr))
      else None
    ) in
  let gammas =
    gammas |> List.map (fun envr ->
      Env_refinement.rm x envr
    ) in
  (VarAnnot.union vas, gammas)

(*let typeof_nofail tenv env e =
  try typeof tenv env e
  with Ill_typed _ -> assert false*)

let typeof_a_nofail pos tenv env a =
  try typeof_a pos tenv env a
  with Ill_typed _ -> assert false

(*type infer_res = e * (Env_refinement.t list) * bool (* Changes? *)*)

let rec infer_legacy' tenv env e t =
  let envr = Env_refinement.empty env in
  match e with
  | Var v -> (e, [Env_refinement.refine_existing v t envr] |> filter_options, false)
  | Bind (va, v, a, e) ->
    log "@,@[<v 1>BIND for variable %a" Variable.pp v ;
    let pos = Variable.get_locations v in
    let splits = VarAnnot.splits env va in
    let res =
      if splits = []
      then begin (* BindArgSkip *)
        log "@,Skipping definition." ;
        let (e, gammas, changes) = infer_legacy' tenv env e t in
        (Bind (VarAnnot.empty, v, empty_annots_a a, e), gammas, changes)
      end else begin
        let dom_a = disj splits in
        let dom_a = (* NOTE: Not in the paper. Used to forward type information for explicitely typed lambdas with multiple arguments *)
          if e = Var v
          then cap_o dom_a t
          else dom_a
        in
        let (a, gammas_a, changes) = infer_legacy_a' pos tenv env a dom_a in
        if gammas_a = []
        then begin (* BindArgUntyp *)
          log "@,Skipping definition." ;
          let (e, gammas, changes) = infer_legacy' tenv env e t in
          (Bind (VarAnnot.empty, v, a (* Should be empty already *), e), gammas, changes (* true *) (* Optimisation *))
        end else if List.exists (fun envr -> Env_refinement.is_empty envr |> not) gammas_a
        then begin (* BindArgRefEnv *)
          log "@,The definition need refinements (going up)." ;
          let gammas =
            if List.exists Env_refinement.is_empty gammas_a
            then gammas_a else envr::gammas_a in
          let e = restrict_annots_e env e in
          let va = VarAnnot.restrict env va in
          (Bind (va, v, a, e), gammas, false (* We made no change to the annotations yet *))
        end else if changes then begin (* BindArgRefAnns *)
          log "@,The definition need a new iteration." ;
          infer_legacy' tenv env (Bind (va, v, a, e)) t
        end else begin (* Bind *)
          log "@,The definition has been successfully annotated." ;
          let s = typeof_a_nofail pos tenv env a in
          assert (subtype s dom_a) ;
          let splits = partition s splits in
          log "@,Using the following split: %a" (Utils.pp_list Cduce.pp_typ) splits ;
          let res =
            splits |> List.map (fun s ->
              let (e, gammas, changes) = infer_legacy' tenv (Env.add v s env) e t in
              let changes =
                if List.length gammas >= 1 && List.for_all Env_refinement.is_empty gammas
                then changes (* The current annotation will not change *)
                else true (* The current annotation (or a parent) will change *)
              in
              let gammas = propagate tenv v a gammas in
              let (va, gammas) = extract v gammas in
              (va, e, gammas, changes)
            ) in
          let (vas, es, gammass, changess) = split4 res in
          let va = VarAnnot.union vas in
          let e = merge_annots_e es in
          let gammas = List.flatten gammass in
          let changes = List.exists identity changess in
          (Bind (va, v, a, e), gammas, changes)
        end
      end
    in
    log "@]@,END BIND for variable %a" Variable.pp v ; res

and infer_legacy_a' (*pos*)_ tenv env a t =
  let envr = Env_refinement.empty env in
  let type_lambda va lt v e t ~maxdom =
    log "@,@[<v 1>LAMBDA for variable %a with t=%a" Variable.pp v pp_typ t ;
    let t = cap_o t arrow_any in
    let res =
      match dnf t |> simplify_dnf with
      | [arrows] -> (* Abs *)
        (* NOTE: Here we ignore the negative part, though we should check there is no negative part.
        But it would require a better simplification of union of arrow types to make negative parts disappear. *)
        let splits1 = VarAnnot.splits env va in
        let splits2 = List.map fst arrows in
        if splits1 = [] || (subtype (disj splits2) (disj splits1) |> not)
        then (Lambda (VarAnnot.empty, lt, v, empty_annots_e e), [], false (* Optimisation *))
        else
          let splits = splits1@splits2 in
          let splits = List.map (fun s -> cap_o s maxdom) splits in
          let splits = partition_for_full_domain splits in
          log "@,Using the following split: %a" (Utils.pp_list Cduce.pp_typ) splits ;
          let res =
            splits |> List.map (fun si ->
              let (e, gammas, changes) = infer_legacy' tenv (Env.add v si env) e (apply_opt t si) in
              let changes =
                if List.length gammas >= 1 && List.for_all Env_refinement.is_empty gammas
                then changes (* The current annotation will not change *)
                else true (* The current annotation (or a parent) will change *)
              in
              let (va, gammas) = extract v gammas in
              (va, e, gammas, changes)
            ) in
          let (vas, es, gammass, changess) = split4 res in
          let va = VarAnnot.union vas in
          let e = merge_annots_e es in
          let gammas = List.flatten gammass in
          let changes = List.exists identity changess in
          if subtype (domain t) (VarAnnot.full_domain va)
          then (Lambda (va, lt, v, e), gammas, changes)
          else (Lambda (VarAnnot.empty, lt, v, empty_annots_e e), [], false (* Optimisation *))
      | lst -> (* AbsUntypeable *)
        if lst <> [] then Format.printf "Warning: An AbsUnion rule would be needed..." ;
        (Lambda (VarAnnot.empty, lt, v, empty_annots_e e), [], false (* Optimisation *))
      in
      log "@]@,END LAMBDA for variable %a" Variable.pp v ; res
  in
  begin match a with
  | Abstract s when subtype s t -> (a, [envr], false)
  | Abstract _ -> (a, [], false)
  | Const c when subtype (typeof_const_atom tenv c) t -> (a, [envr], false)
  | Const _ -> (a, [], false)
  | Pair (v1, v2) ->
    if Env.mem v1 env && Env.mem v2 env then begin
      if is_empty (Env.find v1 env) || is_empty (Env.find v2 env)
      then (a, [envr], false)
      else
        let t = cap_o t pair_any in
        let gammas =
          split_pair t
          |> List.filter_map (fun (ti,si) ->
            envr |>
            option_chain [Env_refinement.refine_existing v1 ti ; Env_refinement.refine_existing v2 si]
          )
        in
        (a, gammas, false)
    end else (a, [], false)
  | Projection ((Fst|Snd), v) ->
    if Env.mem v env then begin
      let vt = Env.find v env in
      if is_empty vt then (a, [envr], false)
      else
        let t =
          match a with
          | Projection (Fst, _) -> mk_times (cons t) any_node
          | Projection (Snd, _) -> mk_times any_node (cons t)
          | _ -> assert false
        in
        let gammas =
          split_pair (cap_o vt t)
          |> List.filter_map (fun (ti,si) ->
            let t = mk_times (cons ti) (cons si) in
            Env_refinement.refine_existing v t envr
          )
        in
        (a, gammas, false)
    end else (a, [], false)
  | Projection (Field label, v) ->
    if Env.mem v env then begin
      let vt = Env.find v env in
      if is_empty vt then (a, [envr], false)
      else
        let t = mk_record true [label, cons t] in
        let gammas =
          split_record (cap_o vt t)
          |> List.filter_map (fun ti ->
            Env_refinement.refine_existing v ti envr
          )
        in
        (a, gammas, false)
    end else (a, [], false)
  | RecordUpdate (v, label, None) ->
    if Env.mem v env then begin
      let vt = Env.find v env in
      if is_empty vt then (a, [envr], false)
      else
        let t = cap_o (record_any_without label) t in
        let gammas =
          split_record t
          |> List.filter_map (fun ti ->
            let ti = remove_field_info ti label in
            Env_refinement.refine_existing v ti envr
          )
        in
        (a, gammas, false)
    end else (a, [], false)
  | RecordUpdate (v, label, Some f) ->
    if Env.mem v env && Env.mem f env then begin
      let vt = Env.find v env in
      let ft = Env.find f env in
      if is_empty vt || is_empty ft then (a, [envr], false)
      else
        let t = cap_o (mk_record true [label, cons ft]) t in
        let gammas =
          split_record t
          |> List.filter_map (fun ti ->
            let si = get_field ti label in
            let ti = remove_field_info ti label in
            envr |>
            option_chain [Env_refinement.refine_existing v ti ; Env_refinement.refine_existing f si ]
          )
        in
        (a, gammas, false)
    end else (a, [], false)
  | Ite (v, s, v1, v2) ->
    if Env.mem v env then begin
      let vt = Env.find v env in
      if is_empty vt then (a, [envr], false)
      else
        let gammas =
          [ envr |> option_chain [Env_refinement.refine_existing v s       ; Env_refinement.refine_existing v1 t] ;
            envr |> option_chain [Env_refinement.refine_existing v (neg s) ; Env_refinement.refine_existing v2 t] ]
          |> filter_options
        in
        (a, gammas, false)
    end else (a, [], false)
  | App (v1, v2) ->
    if Env.mem v1 env && Env.mem v2 env then begin
      let vt1 = Env.find v1 env in
      let vt2 = Env.find v2 env in
      if is_empty vt1 || (is_empty vt2 && subtype vt1 arrow_any)
      then (a, [envr], false)
      else
        let vt1 = cap_o vt1 arrow_any in
        (* NOTE: In the paper, the rule AppR does not interstect vt1 with arrow_any *)
        match dnf vt1 |> simplify_dnf with
        | [arrows] -> (* AppR *)
          let gammas =
            arrows |> List.filter_map (fun (si,_) ->
              let arrow_type = mk_arrow (cons (cap_o si vt2)) (cons t) in
              envr |> option_chain [
                Env_refinement.refine_existing v1 arrow_type ; Env_refinement.refine_existing v2 si
              ]
            ) in
          (a, gammas, false)
        | lst -> (* AppL *)
          let gammas =
            lst |> List.filter_map (fun arrows ->
              Env_refinement.refine_existing v1 (branch_type arrows) envr
            ) in
          (a, gammas, false)
    end else (a, [], false)
  | Let (v1, v2) ->
    let gammas =
      [envr |> option_chain
        [Env_refinement.refine_existing v1 any ; Env_refinement.refine_existing v2 t ]]
      |> filter_options in
      (a, gammas, false)
  | Lambda (va, (Ast.ADomain s as lt), v, e) ->
    let t = cap_o t (mk_arrow (cons s) any_node) in
    type_lambda va lt v e t ~maxdom:s
  | Lambda (va, (Ast.Unnanoted as lt), v, e) ->
    let t = cap_o t arrow_any in
    type_lambda va lt v e t ~maxdom:any
  | Lambda (va, (Ast.AArrow s as lt), v, e) ->
    let t = cap_o t s in
    type_lambda va lt v e t ~maxdom:(domain s)
  end

let rec infer_legacy_iterated tenv e =
  match infer_legacy' tenv Env.empty e any with
  | (_, [], _) -> raise (Ill_typed ([], "Annotations inference failed."))
  | (e, _, false) -> e
  | (e, _, true) -> infer_legacy_iterated tenv e

let infer_legacy tenv env e =
  let fv = fv_e e in
  let e = VarSet.fold (fun v acc ->
    Bind (VarAnnot.initial_binding ~legacy:true, v, Abstract (var_type [] v env), acc)
  ) fv e in
  let e = infer_legacy_iterated tenv e in
  log "@." ; e

let typeof_simple_legacy tenv env e =
  let e = infer_legacy tenv env e in
  typeof tenv Env.empty e |> simplify_typ

(* ========== NEW SYSTEM (LAZY) ========== *)

let rec infer_a' (*pos*)_ tenv env a t =
  ignore tenv ; ignore env ; ignore a ; ignore t ;
  failwith "TODO"

and infer' tenv env e t =
  ignore infer_a' ; ignore tenv ; ignore env ; ignore e ; ignore t ;
  let envr = Env_refinement.empty env in
  match e with
  | Var v -> (e, [Env_refinement.refine v t envr] |> filter_options, false)
  | Bind (va, v, a, e) ->
    log "@,@[<v 1>BIND for variable %a" Variable.pp v ;
    (*let pos = Variable.get_locations v in*)
    let splits = VarAnnot.splits env va in
    let res =
      if splits = []
      then begin (* BindArgSkip *)
        log "@,Skipping definition." ;
        let (e, gammas, changes) = infer_iterated tenv env e t in
        let (va, gammas) = extract v gammas in
        let changes = changes || (VarAnnot.is_empty va |> not) in
        (Bind (va, v, restrict_annots_a env a, e), gammas, changes)
      end else begin
        failwith "TODO"
      end
    in
    log "@]@,END BIND for variable %a" Variable.pp v ; res

and infer_iterated tenv env e t =
  match infer' tenv env e t with
  | (e, [env'], true) when Env_refinement.is_empty env' -> infer_iterated tenv env e t
  | (e, gammas, b) -> (e, gammas, b)

let infer tenv env e =
  let fv = fv_e e in
  let e = VarSet.fold (fun v acc ->
    Bind (VarAnnot.initial_binding ~legacy:false, v, Abstract (var_type [] v env), acc)
  ) fv e in
  let e =
    match infer_iterated tenv Env.empty e any with
    | (_, [] , _) -> raise (Ill_typed ([], "Annotations inference failed."))
    | (e, _, _) -> e
  in
  log "@." ; e

let typeof_simple tenv env e =
  let e = infer tenv env e in
  typeof tenv Env.empty e |> simplify_typ
