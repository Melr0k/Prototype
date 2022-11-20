open Base
open Tvar

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)
module LabelMap = CD.Ident.LabelMap

exception TypeDefinitionError of string

(* Construction of types *)

type type_base =
    | TInt of int option * int option | TSChar of char | TSString of string
    | TBool | TTrue | TFalse | TUnit | TChar | TAny | TEmpty | TNil
    | TString | TList

type type_regexp =
    | ReEpsilon | ReEmpty
    | ReType of type_expr
    | ReSeq of type_regexp * type_regexp
    | ReStar of type_regexp
    | ReAlt of type_regexp * type_regexp

and type_expr =
    | TVar of string
    | TBase of type_base
    | TCustom of type_expr list * string
    | TPair of type_expr * type_expr
    | TRecord of bool * (string * type_expr * bool) list
    | TSList of type_regexp
    | TArrow of type_expr * type_expr
    | TCup of type_expr * type_expr
    | TCap of type_expr * type_expr
    | TDiff of type_expr * type_expr
    | TNeg of type_expr

type type_alias = TVar.t list * node
type type_env = type_alias StrMap.t (* User-defined types *) * StrSet.t (* Atoms *)
type var_type_env = typ StrMap.t (* Var types *)

let empty_tenv = (StrMap.empty, StrSet.empty)
let empty_vtenv = StrMap.empty

let type_base_to_typ t =
    match t with
    | TInt (lb,ub) -> interval lb ub
    | TSChar c -> single_char c
    | TSString str -> single_string str
    | TBool -> bool_typ | TNil -> nil_typ
    | TTrue -> true_typ | TFalse -> false_typ
    | TUnit -> unit_typ | TChar -> char_typ
    | TAny -> any | TEmpty -> empty
    | TString -> string_typ | TList -> list_typ

let instantiate_alias env args name =
    try
        let (params, node) = StrMap.find name env in
        let subst = List.combine params args |> Subst.construct in
        Subst.apply subst (descr node)
    with
    | Not_found -> raise (TypeDefinitionError (Printf.sprintf "Type %s undefined!" name))
    | Invalid_argument _ -> raise (TypeDefinitionError (Printf.sprintf "Wrong arity for type %s!" name))

let derecurse_types env venv defs =
    let open Cduce_core in
    let venv =
        let h = Hashtbl.create 16 in
        StrMap.iter (fun n v -> Hashtbl.add h n v) venv ;
        h
    in
    let henv = Hashtbl.create 16 in
    let () =
        List.iter (fun (name, params, def) ->
                if StrMap.mem name env then 
                    raise (TypeDefinitionError (Printf.sprintf "Type %s already defined!" name))
                else
                    Hashtbl.add henv name (def, params, [])) defs
    in
    let rec get_name ~nd args name =
        match Hashtbl.find henv name with
        | def, params, lst ->
            if nd then raise (TypeDefinitionError (Printf.sprintf "Cannot use a reference to %s here!" name)) ;
            let cached = lst |> List.find_opt (fun (args',_) -> List.for_all2 equiv args args') in
            begin match cached with
            | None ->
                let v = Typepat.mk_delayed () in
                Hashtbl.replace henv name (def, params, (args, v)::lst);
                let local = List.combine params args |> List.to_seq |> StrMap.of_seq in
                let t = aux ~nd local def in
                Typepat.link v t;
                v
            | Some (_, v) -> v
            end
        | exception Not_found -> 
            Typepat.mk_type (instantiate_alias env args name)
        | exception Invalid_argument _ ->
            raise (TypeDefinitionError (Printf.sprintf "Wrong arity for type %s!" name))
    and aux ~nd (* no delayed: disallow relying on delayed vars *) lcl t =
        match t with
        | TVar v ->
            begin match StrMap.find_opt v lcl, Hashtbl.find_opt venv v with
            | Some t, _ | None, Some t -> Typepat.mk_type t
            | None, None ->
                let t = TVar.mk_mono (Some v) |> TVar.typ in
                Hashtbl.add venv v t ;
                Typepat.mk_type t
            end
        | TBase tb -> Typepat.mk_type (type_base_to_typ tb)
        | TCustom (args, n) ->
            let args = args |> List.map (aux ~nd:true lcl) |> List.map Typepat.typ in
            get_name ~nd args n
        | TPair (t1,t2) -> Typepat.mk_prod (aux ~nd lcl t1) (aux ~nd lcl t2)
        | TRecord (is_open, fields) ->
            let aux' (label,t,opt) =
                let n = aux ~nd lcl t in
                let n = if opt then Typepat.mk_optional n else n in
                (to_label label, (n, None))
            in
            let lmap = 
                Cduce_types.Ident.LabelMap.from_list_disj (List.map aux' fields)
            in
            Typepat.mk_record is_open lmap
        | TSList lst -> Typepat.rexp (aux_re ~nd lcl lst)
        | TArrow (t1,t2) -> Typepat.mk_arrow (aux ~nd lcl t1) (aux ~nd lcl t2)
        | TCup (t1,t2) ->
            let t1 = aux ~nd lcl t1 in
            let t2 = aux ~nd lcl t2 in
            Typepat.mk_or t1 t2
        | TCap (t1,t2) ->
            let t1 = aux ~nd lcl t1 in
            let t2 = aux ~nd lcl t2 in
            Typepat.mk_and t1 t2
        | TDiff (t1,t2) ->
            let t1 = aux ~nd lcl t1 in
            let t2 = aux ~nd lcl t2 in
            Typepat.mk_diff t1 t2
        | TNeg t -> Typepat.mk_diff (Typepat.mk_type any) (aux ~nd lcl t)
    and aux_re ~nd lcl r =
        match r with
        | ReEmpty -> Typepat.mk_empty
        | ReEpsilon -> Typepat.mk_epsilon
        | ReType t -> Typepat.mk_elem (aux ~nd lcl t)
        | ReSeq (r1, r2) -> Typepat.mk_seq (aux_re ~nd lcl r1) (aux_re ~nd lcl r2)
        | ReAlt (r1, r2) -> Typepat.mk_alt (aux_re ~nd lcl r1) (aux_re ~nd lcl r2)
        | ReStar r -> Typepat.mk_star (aux_re ~nd lcl r)
    in
    let res = defs |> List.map (fun (name, params, _) ->
        let params = List.map (fun _ -> TVar.mk_unregistered ()) params in
        let args = List.map TVar.typ params in
        let node = get_name ~nd:false args name in
        (* Typepat.internalize node ; *)
        name, params, Typepat.typ node) in
    let venv = Hashtbl.fold StrMap.add venv StrMap.empty in
    (res, venv)

let type_expr_to_typ (tenv, _) venv t = 
    match derecurse_types tenv venv [ ("", [], t) ] with
    | ([ _, _, n ], venv) -> (n, venv)
    | _ -> assert false

let define_types (tenv, aenv) venv defs =
    let defs = List.map
        (fun (name, params, decl) -> (String.capitalize_ascii name, params, decl))
        defs
    in
    let (res, venv) = derecurse_types tenv venv defs in
    let tenv = List.fold_left
        (fun acc (name, params, typ) ->
            if params = [] then register name typ ;
            StrMap.add name (params, cons typ) acc)
        tenv
        res
    in ((tenv, aenv), venv)

let define_atom (env, atoms) name =
    let atom = String.uncapitalize_ascii name in
    let typ = String.capitalize_ascii name in
    if StrMap.mem typ env
    then raise (TypeDefinitionError (Printf.sprintf "Type %s already defined!" typ))
    else (StrMap.add typ ([], cons (mk_atom atom)) env, StrSet.add atom atoms)

let get_atom_type (env, _) name =
    let name = String.capitalize_ascii name in
    instantiate_alias env [] name

let has_atom (_, atoms) name =
    let name = String.uncapitalize_ascii name in
    StrSet.mem name atoms

(* Operations on types *)

let conj ts = List.fold_left cap any ts
let disj ts = List.fold_left cup empty ts
let conj_o ts = List.fold_left cap_o any ts
let disj_o ts = List.fold_left cup_o empty ts

let branch_type lst =
    if lst = [] then arrow_any
    else begin
        lst
        |> List.map (fun (a, b) -> mk_arrow (cons a) (cons b))
        |> conj
    end

let full_branch_type ((pvs, nvs), (ps, ns)) =
    let pvs = pvs |> List.map TVar.typ |> conj in
    let nvs = nvs |> List.map TVar.typ |> List.map neg |> conj in
    let ps = ps |>
        List.map (fun (a, b) -> mk_arrow a b) |> conj in
    let ns = ns |>
        List.map (fun (a, b) -> mk_arrow a b |> neg) |> conj in
    let t = [pvs;nvs;ps;ns] |> conj in
    cap arrow_any t

let full_product_branch_type ((pvs, nvs), (ps, ns)) =
    let pvs = pvs |> List.map TVar.typ |> conj in
    let nvs = nvs |> List.map TVar.typ |> List.map neg |> conj in
    let ps = ps |>
        List.map (fun (a, b) -> mk_times a b) |> conj in
    let ns = ns |>
        List.map (fun (a, b) -> mk_times a b |> neg) |> conj in
    let t = [pvs;nvs;ps;ns] |> conj in
    cap pair_any t

let rec take_one lst =
    match lst with
    | [] -> []
    | e::lst ->
        (e, lst)::(List.map (fun (e',lst) -> (e',e::lst)) (take_one lst))

module NHT = Hashtbl.Make(CD.Types.Node)
let rec regroup_conjuncts ~open_nodes conjuncts =
    let rec aux (l,r) lst = match lst with
    | [] -> ((l,r), [])
    | (l',r')::lst ->
        if (NHT.mem open_nodes r |> not) && (NHT.mem open_nodes r' |> not)
            && equiv (descr l) (descr l')
        then aux (l, cap (descr r) (descr r') |> cons) lst
        else if (NHT.mem open_nodes l |> not) && (NHT.mem open_nodes l' |> not)
            && equiv (descr r) (descr r')
        then aux (cup (descr l) (descr l') |> cons, r) lst
        else
            let ((l,r),lst) = aux (l,r) lst in
            ((l,r), (l',r')::lst)
    in
    match conjuncts with
    | [] -> []
    | (l, r)::lst ->
        let ((l,r),lst) = aux (l,r) lst in
        (l,r)::(regroup_conjuncts ~open_nodes lst)

let regroup_conjuncts_descr ps =
    ps |>
    List.map (fun (a,b) -> (cons a, cons b)) |>
    regroup_conjuncts ~open_nodes:(NHT.create 0) |>
    List.map (fun (a,b) -> (descr a, descr b))

let simplify_dnf dnf =
    let splits = List.map branch_type dnf in
    let splits = List.combine dnf splits in
    let rec rm f kept lst = match lst with
    | [] -> kept
    | (dnf, t)::lst ->
        let (_, ts1) = List.split lst in
        let (_, ts2) = List.split kept in
        if f t (ts1@ts2) then rm f kept lst else rm f ((dnf, t)::kept) lst
    in
    let simplify_conjuncts (conjuncts, _) =
        let conjuncts = conjuncts |>
            List.map (fun (a, b) -> ((a,b), mk_arrow (cons a) (cons b))) |>
            rm (fun t ts -> subtype (conj ts) t) [] (* Remove redundant conjuncts *)
        in
        (* Regroup conjuncts with similar domain/codomain *)
        conjuncts |> List.split |> fst |> regroup_conjuncts_descr
    in
    rm (fun t ts -> subtype t (disj ts)) [] splits
    |> List.map simplify_conjuncts        

let remove_useless_conjuncts branch_type ~n dc cc lst =
    let atom_type (a,b) =
        if n then branch_type (([],[]),([],[(a,b)]))
        else branch_type (([],[]),([(a,b)],[]))
    in
    let rec aux kept rem =
        match rem with
        | [] -> kept
        | c::rem ->
            let ct = atom_type c in
            let rt = rem |> List.map atom_type |> conj in
            let kt = kept |> List.map atom_type |> conj in
            let others = conj [kt ; rt ; cc] in
            let t' = cup dc others in
            let t  = cup dc (cap others ct) in
            if subtype t' t then aux kept rem
            else aux (c::kept) rem
    in
    aux [] lst

let remove_useless_conjuncts branch_type dc ((pvs, nvs), (ps, ns)) =
    let context = branch_type ((pvs, nvs), ([], ns)) in
    let ps = remove_useless_conjuncts branch_type ~n:false dc context ps in
    let context = branch_type ((pvs, nvs), (ps, [])) in
    let ns = remove_useless_conjuncts branch_type ~n:true dc context ns in
    ((pvs, nvs), (ps, ns))

let remove_useless_from_dnf branch_type dnf =
    (* Remove useless conjuncts *)
    let rec aux treated rem =
        match rem with
        | [] -> treated
        | c::rem ->
            let rt = rem |> List.map branch_type |> disj in
            let tt = treated |> List.map branch_type |> disj in
            let others = cup tt rt in
            let c = remove_useless_conjuncts branch_type others c in
            aux (c::treated) rem
    in
    let dnf = aux [] dnf in
    (* Remove useless disjuncts *)
    let rec aux kept rem =
        match rem with
        | [] -> kept
        | c::rem ->
            let ct = branch_type c in
            let rt = rem |> List.map branch_type |> disj in
            let kt = kept |> List.map branch_type |> disj in
            let others = cup kt rt in
            if subtype ct others then aux kept rem
            else aux (c::kept) rem
    in
    aux [] dnf

let [@warning "-27"] simplify_raw_dnf _ ~open_nodes ~contravar dnf =
    let regroup_conjuncts (vars, (ps, ns)) =
        (vars, (regroup_conjuncts ~open_nodes ps, ns))
    in
    let dnf = remove_useless_from_dnf full_branch_type dnf in
    (* Regroup positive conjuncts with similar domain/codomain  *)
    List.map regroup_conjuncts dnf

let [@warning "-27"] simplify_raw_product_dnf _ ~open_nodes ~contravar dnf =
    let dnf = remove_useless_from_dnf full_product_branch_type dnf in
    (* TODO: More advanced simplifications for products *)
    dnf

let is_test_type t =
    if vars t |> TVarSet.is_empty
    then
        let is_non_trivial_arrow t =
            let arrow_part = cap t arrow_any in
            (is_empty arrow_part || subtype arrow_any arrow_part) |> not
        in
        let memo = Hashtbl.create 10 in
        let rec aux t =
            try Hashtbl.find memo t
            with Not_found -> begin
                if is_non_trivial_arrow t
                then (Hashtbl.add memo t false ; false)
                else begin
                    Hashtbl.add memo t true ;
                    split_pair t |>
                    List.for_all (fun (x,y) -> aux x && aux y)
                end
            end
        in aux t
    else false

let pair_vars (a,b) = TVarSet.union (vars (descr a)) (vars (descr b))
let pairs_vars lst =
    lst |> List.map pair_vars |>
    List.fold_left TVarSet.union TVarSet.empty
let branch_vars ((pvs, nvs), (ps,ns)) =
    TVarSet.construct (pvs@nvs) |>
    TVarSet.union (pairs_vars ps) |>
    TVarSet.union (pairs_vars ns)
let branches_vars lst =
    lst |> List.map branch_vars |>
    List.fold_left TVarSet.union TVarSet.empty

let simplify_typ_aux simplify_arrow simplify_product mono t =
    (*Utils.log ~level:2 "Simplifying type...@?" ;*)
    let cache = NHT.create 5 in
    let rec aux mono contravar node =
        let aux_pair arrow mono (a,b) =
            let monoa = TVarSet.union mono (vars (descr a)) in
            let monob = TVarSet.union mono (vars (descr b)) in
            (aux monob (arrow <> contravar) a, aux monoa contravar b) in
        let aux_pairs arrow mono ps =
            ps |> Utils.add_others |> List.map (fun (ps, others) ->
                let vs = pairs_vars others in
                aux_pair arrow (TVarSet.union mono vs) ps
            )
        in
        let aux_branch arrow mono ((pvs, nvs), (ps,ns)) =
            let mono = TVarSet.construct (pvs@nvs) |> TVarSet.union mono in
            let ps_vars = ps |> pairs_vars in
            let ns_vars = ns |> pairs_vars in
            let ps = aux_pairs arrow (TVarSet.union mono ns_vars) ps in
            let ns = aux_pairs arrow (TVarSet.union mono ps_vars) ns in
            ((pvs,nvs),(ps,ns))
        in
        let aux_branches arrow mono lst =
            lst |> Utils.add_others |> List.map (fun (branch, others) ->
                let vs = branches_vars others in
                aux_branch arrow (TVarSet.union mono vs) branch
            )
        in
        match NHT.find_opt cache node with
        | Some n -> n
        | None ->
            let n = mk_new_typ () in
            NHT.add cache node n ;
            let open Iter in
            let t = fold (fun acc pack t ->
                let t = match pack with
                | Absent -> absent
                | Abstract m | Char m | Int m | Atom m ->
                    let module K = (val m : Kind) in
                    K.get_vars t |> K.mk
                | Times m ->
                    let module K = (val m) in
                    K.get_vars t |> K.Dnf.get_full
                    |> simplify_product mono ~open_nodes:cache ~contravar
                    |> aux_branches false mono
                    |> List.map full_product_branch_type |> disj
                | Xml m ->
                    let module K = (val m) in
                    K.get_vars t |> K.mk
                | Function m ->
                    let module K = (val m) in
                    K.get_vars t |> K.Dnf.get_full
                    |> simplify_arrow mono ~open_nodes:cache ~contravar
                    |> aux_branches true mono
                    |> List.map full_branch_type |> disj
                | Record m ->
                    let module K = (val m) in
                    let dnf = K.get_vars t in
                    (* TODO: Implement simplify_typ for records *)
                    K.mk dnf
                in
                cup acc t
            ) empty (descr node) in
            define_typ n t ; n
    in
    let res = aux mono false (cons t) |> descr in
    (* TODO: Uncomment the assert and fix it. *)
    (* if equiv res t |> not then Format.printf "Before:%a@.After:%a@." pp_typ t pp_typ res ; *)
    (* assert (equiv res t) ; *)
    (* Utils.log ~level:2 " Done.@." ;*)
    res

let simplify_typ = simplify_typ_aux
    simplify_raw_dnf simplify_raw_product_dnf TVarSet.empty

let square_approx f out =
    let res = dnf f |> List.map begin
        fun lst ->
            let is_impossible (_,t) = is_empty (cap out t) in
            let impossibles = List.filter is_impossible lst |> List.map fst in
            neg (disj impossibles)
    end in
    cap (domain f) (disj res)

let square_exact f out =
    assert (is_empty out |> not) ;
    let res = dnf f |> List.map begin
        fun lst ->
            let remove_included_branchs lst =
                let is_not_included (_, o) = subtype o out |> not in
                List.filter is_not_included lst
            in
            let rec impossible_inputs current_set lst =
                let t = List.map snd current_set in
                if disjoint (conj t) out then [conj (List.map fst current_set)]
                else begin
                    let aux (e,lst) = impossible_inputs (e::current_set) lst in
                    List.flatten (List.map aux (take_one lst))
                end
            in
            conj (List.map neg (impossible_inputs [] (remove_included_branchs lst)))
    end in
    cap (domain f) (disj res)

let square_split f out =
    dnf f |>
    List.map begin
        fun lst ->
            let t = branch_type lst in
            let res = square_exact t out in
            (t, res)
    end

let triangle_exact f out =
    let res = dnf f |> List.map begin
        fun lst ->
            (*let remove_disjoint_branchs lst =
                let is_not_disjoint (_, o) = disjoint o out |> not in
                List.filter is_not_disjoint lst
            in*)
            let rec possible_inputs current_set lst =
                let t = List.map snd current_set in
                if t <> [] && subtype (conj t) out then [conj (List.map fst current_set)]
                else begin
                    let aux (e,lst) = possible_inputs (e::current_set) lst in
                    List.flatten (List.map aux (take_one lst))
                end
            in
            disj (possible_inputs [] ((*remove_disjoint_branchs*)lst))
    end in
    conj res

let triangle_split f out =
    dnf f |>
    List.map begin
        fun lst ->
            let t = branch_type lst in
            let res = triangle_exact t out in
            (t, res)
    end

(* Record manipulation *)
let record_any_with l = mk_record true [l, any_node]

let record_any_without l = mk_record true [l, (or_absent empty |> cons)]

let split_record t =
  let to_node (is_absent, t) =
    if is_absent then
      cons (CD.Types.Record.or_absent t)
    else
      cons t
  in
  let to_record (labels, is_open, _) =
    let labels = LabelMap.map to_node labels in
    CD.Types.record_fields (is_open, labels)
  in
  CD.Types.Record.get t |> List.map to_record

let remove_field_info t label =
    let t = remove_field t label in
    let singleton = mk_record false [label, any_or_absent_node] in
    merge_records t singleton

(* Operations on substs and vars *)

module type Subst = sig
    include Subst
    val find' : t -> TVar.t -> typ
    val compose : t -> t -> t
    val compose_restr : t -> t -> t
    val combine : t -> t -> t
    val restrict : t -> TVarSet.t -> t
    val remove : t -> TVarSet.t -> t
    val split : t -> TVarSet.t -> t * t
    val apply_simplify : t -> typ -> typ
    val codom : t -> TVarSet.t
end
module Subst : Subst = struct
    include Subst
    let find' t v =
        if mem t v then find t v else TVar.typ v
    let compose_restr_ s2 s1 =
        destruct s1 |>
            List.map (fun (v,t) -> (v, apply s2 t))
    let compose_restr s2 s1 = compose_restr_ s2 s1 |> construct
    let compose s2 s1 =
        let only_s2 = destruct s2 |>
            List.filter (fun (v, _) -> mem s1 v |> not) in
        construct ((compose_restr_ s2 s1)@only_s2)
    let combine s1 s2 =
        assert (TVarSet.inter (dom s1) (dom s2) |> TVarSet.is_empty) ;
        let s1 = destruct s1 in
        let s2 = destruct s2 in
        s1@s2 |> construct
    let restrict s vars =
        let vars = TVarSet.inter (dom s) vars in
        vars |> TVarSet.destruct |> List.map (fun v -> (v, find s v)) |> construct
    let remove s vars =
        let nvars = TVarSet.diff (dom s) vars in
        restrict s nvars
    let split s vars =
        (restrict s vars, remove s vars)
    let apply_simplify s t =
        if TVarSet.inter (Subst.dom s) (vars t) |> TVarSet.is_empty
        then t else Subst.apply s t |> simplify_typ
    let codom s =
        destruct s |> List.map (fun (_, t) -> vars t)
        |> List.fold_left TVarSet.union TVarSet.empty
end

let clean_poly_vars mono t =
    clean_type ~pos:empty ~neg:any mono t

let clean_type_ext ~pos ~neg mono t =
    let subst =
        vars_with_polarity t |>
        List.filter_map (fun (v,p) ->
            if TVarSet.mem mono v then None
            else match p with
            | `Pos -> Some (v, pos)
            | `Neg -> Some (v, neg)
            | `Both -> None
        )
    in
    Subst.construct subst

let instantiate ss t =
    List.map (fun s -> Subst.apply_simplify s t) ss
    |> conj

let fresh mono t =
    let poly = TVarSet.diff (vars t) mono in
    let subst = refresh_all poly in
    let x = Subst.codom subst in
    (x, subst, Subst.apply subst t)

let print_tallying_instance var_order delta constr =
    Format.printf "Constraints:@." ;
    constr |> List.iter (fun (l,r) ->
        Format.printf "(%a, %a)@." pp_typ l pp_typ r ;
    );
    Format.printf "With delta=%a and var order=%a@."
        (Utils.pp_list TVar.pp) (TVarSet.destruct delta)
        (Utils.pp_list TVar.pp) var_order

let check_tallying_solution var_order delta constr res =
    let error = ref false in
    let res =
        res |> List.filter_map (fun s ->
        if (constr |> List.for_all (fun (l,r) ->
                subtype (Subst.apply s l) (Subst.apply s r)
            ))
        then Some s else begin
            error := true ;
            Format.printf "INVALID SOLUTION REMOVED: %a@." Subst.pp s ;
            None
        end
    )
    in
    if !error then begin
        Format.printf "WARNING: Cduce tallying issue.@." ;
        print_tallying_instance var_order delta constr
    end ; res

let tallying_infer poly noninfered constr =
    assert (TVarSet.inter (TVarSet.construct poly) noninfered |> TVarSet.is_empty) ;
    Utils.log ~level:2 "Tallying (inference) instance initiated...@?" ;
    let res = tallying ~var_order:poly noninfered constr in
    Utils.log ~level:2 " Done (%i sol).@." (List.length res) ;
    res |> check_tallying_solution poly noninfered constr

let tallying mono constr =
    Utils.log ~level:2 "Tallying (no inference) instance initiated...@?" ;
    let var_order = [] in
    let res = tallying ~var_order mono constr in
    Utils.log ~level:2 " Done (%i sol).@." (List.length res) ;
    res |> check_tallying_solution [] mono constr

let subtype_poly mono t1 t2 =
    let (xs, _, t) = fresh mono t2 in
    let res = tallying (TVarSet.union mono xs) [(t1,t)] in
    res <> []

let triangle_poly mono t s =
    (* Utils.log "Triangle_poly with t=%a and s=%a@." pp_typ t pp_typ s ; *)
    let (vt',_,t') = fresh mono t in
    let alpha = TVar.mk_poly None in
    let delta = TVarSet.union mono (vars s) in
    let res = tallying_infer (TVarSet.destruct vt') delta
        [(t', mk_arrow (TVar.typ alpha |> cons) (cons s))] in
    res |> List.map (fun subst ->
        let res = Subst.find' subst alpha |> sup_typ delta in
        (* Utils.log "Solution:%a@." pp_typ res ; *)
        res
    ) |> disj

let triangle_split_poly mono f out =
    dnf f |>
    List.map begin
        fun lst ->
            let t = branch_type lst in
            let (_,_,t) = fresh mono t in
            (sup_typ mono t, triangle_poly mono t out)
    end

(* Simplification of polymorphic types *)

let remove_redundant_vars mono t =
    let compose_res (s,t) res = match res with
    | None -> (Subst.identity, t)
    | Some (s', t') -> (Subst.compose s' s, t')
    in
    let rec aux t =
        let vs = TVarSet.diff (vars t) mono |> TVarSet.destruct in
        Utils.pairs vs vs
        |> List.filter (fun (v1, v2) -> TVar.compare v1 v2 < 0)
        |> List.find_opt (fun (v1, v2) ->
            let v1' = TVar.mk_fresh v1 in
            let v2' = TVar.mk_fresh v2 in
            let subst1 = Subst.construct [(v1, TVar.typ v1');(v2, TVar.typ v2')] in
            let subst2 = Subst.construct [(v1, TVar.typ v2');(v2, TVar.typ v1')] in
            let t1 = Subst.apply subst1 t in
            let t2 = Subst.apply subst2 t in
            equiv t1 t2
        )
        |> Option.map (fun (v1, v2) ->
            let subst = Subst.construct [(v1, TVar.typ v2)] in
            let t = Subst.apply subst t in
            let res = aux t in
            compose_res (subst, t) res
        )
    in
    aux t |> compose_res (Subst.identity, t)

let remove_useless_poly_conjuncts mono branch_type lst =
    let atom_type (a,b) = branch_type (([],[]),([(a,b)],[])) in
    let rec aux kept rem =
        match rem with
        | [] -> kept
        | c::rem ->
            let ct = atom_type c in
            (* let rt = rem |> List.map atom_type |> conj in
            let kt = kept |> List.map atom_type |> conj in
            let others = conj [kt ; rt] in
            if subtype_poly mono others ct *)
            let rt = rem |> List.map atom_type in
            let kt = kept |> List.map atom_type in
            let others = kt@rt in
            if List.exists (fun other -> subtype_poly mono other ct) others
            then aux kept rem else aux (c::kept) rem
    in
    aux [] lst

let [@warning "-27"] simplify_poly_dnf mono ~open_nodes ~contravar dnf =
    let aux mono ((pvs,nvs),(ps,ns)) =
        let tvars = TVarSet.construct (pvs@nvs) in
        let tvars = TVarSet.diff tvars mono in
        if TVarSet.is_empty tvars |> not && not contravar
        then ((pvs,nvs),([],[]))
        else if not contravar then
            (* (ignore remove_useless_poly_conjuncts ; ((pvs,nvs),(ps,ns))) *)
            ((pvs,nvs), (remove_useless_poly_conjuncts mono full_branch_type ps, ns))
        else ((pvs,nvs),(ps,ns))
    in
    Utils.add_others dnf |> List.map (fun (branch, others) ->
        let mono = TVarSet.union mono (branches_vars others) in
        aux mono branch
    )

let [@warning "-27"] simplify_poly_product_dnf mono ~open_nodes ~contravar dnf =
    (* TODO: poly simplifications for products *)
    dnf

let simplify_poly_typ mono t =
    let t = clean_poly_vars mono t in
    let (_, t) = remove_redundant_vars mono t in
    ignore (simplify_poly_dnf, simplify_poly_product_dnf) ;
    (* NOTE: Advanced simplification disabled because it sometimes raise a Cduce issue,
       and it is not very efficient anyway (in particular when branches use the same vars). *)
    (* let t = simplify_typ_aux simplify_poly_dnf simplify_poly_product_dnf mono t in
    let t = clean_poly_vars mono t in
    let (_, t) = remove_redundant_vars mono t in *)
    t

(* Operations on jokers (legacy) *)

module Joker = struct
    type joker_kind = Min | Max
    let reserved_name_for_joker t =
        match t with
        | Min -> "-"
        | Max -> "+"

    let joker k = TVar.mk_mono (Some (reserved_name_for_joker k)) |> TVar.typ
    let jokers k t =
        vars t |> TVarSet.filter (fun v -> String.equal (TVar.display_name v) (reserved_name_for_joker k))
    let top_jokers k t =
        top_vars t |> TVarSet.filter (fun v -> String.equal (TVar.display_name v) (reserved_name_for_joker k))

    let substitute_jokers k t t_subs =
        let subst = jokers k t |> TVarSet.destruct |> List.map (fun j -> (j,t_subs)) |> Subst.construct in
        Subst.apply subst t

    let substitute_all_jokers t t_subs =
        let t = substitute_jokers Min t t_subs in
        substitute_jokers Max t t_subs

    let optimal t =
        let t = substitute_jokers Min t empty in
        substitute_jokers Max t any

    let worst t =
        let t = substitute_jokers Max t empty in
        substitute_jokers Min t any

    let substitute_top_jokers k t t_subs =
        let subst = top_jokers k t |> TVarSet.destruct |> List.map (fun j -> (j,t_subs)) |> Subst.construct in
        Subst.apply subst t

    let required_part_of_branch (a,b) =
        if is_empty a then Some (a, b)
        else
            let js = top_jokers Max a |> TVarSet.destruct in
            let subst = js |> List.map (fun j -> (j, empty)) |> Subst.construct in
            let a = Subst.apply subst a in
            if is_empty a then None else Some (a,b)

    let decompose_branch (a,b) =
        match required_part_of_branch (a,b) with
        | None -> (Some (a,b), None)
        | Some (a',_) ->
            let a = diff a a' in
            let res = if is_empty a then None else Some (a,b) in
            (res, Some (a', b))

    let decompose_branches lst =
        let rec aux lst =
            match lst with
            | [] -> ([], [])
            | b::lst -> begin
                let (js, njs) = aux lst in
                match decompose_branch b with
                | None, None -> (js, njs)
                | None, Some b -> (js, b::njs)
                | Some b, None -> (b::js, njs)
                | Some b1, Some b2 -> (b1::js, b2::njs)
            end
        in
        aux lst

    let extract_jokerized_arrows t =
        dnf t |> List.map decompose_branches |> List.map fst
        |> List.concat

    let add_joker_branch t joker =
        let non_arrow = diff t arrow_any in
        cup non_arrow (cap_o t (branch_type joker))

    let share_jokerized_arrows lst =
        let jokers = lst |> List.map extract_jokerized_arrows in
        lst |> List.map
            (fun t -> List.fold_left add_joker_branch t jokers)
end