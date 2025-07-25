open Types.Base
open Types.Additions
open Variable
open Pomap

exception SymbolError of string
exception LexicalError of Position.t * string
exception SyntaxError of Position.t * string

type varname = string
type exprid = int

type annotation = exprid Position.located

module type Effect = sig
  type e [@@deriving ord]
  val pure : e
  val allocate : e
  val read : e
  val write : e
  val n_pure : e
  val (@&) : e -> e -> e
  val is_pure : e -> bool
end

module Ebool : Effect = struct
  type e = bool [@@deriving ord]
  let pure = true
  let allocate = false
  let read = false
  let write = false
  let n_pure = false
  let (@&) a b = a && b
  let is_pure e = e
end

module Eset : Effect = struct
  module Kind = struct type t = Al | Rd | Wr [@@deriving ord] end
  module ESet = Set.Make (Kind)
  type e = ESet.t [@@deriving ord]
  let pure = ESet.empty
  let allocate = ESet.of_list [Al]
  let read = ESet.of_list [Rd]
  let write = ESet.of_list [Wr]
  let n_pure = ESet.of_list [Al;Rd;Wr]
  let (@&) a b = ESet.union a b
  let is_pure e = (ESet.cardinal e) = 0
end

module type SE_t = sig
  type t [@@deriving ord]
  (* side effects : evaluating e to v, the app v e' etc. *)

  include Effect

  val of_eff : e -> t

  val not_pure : t (* r_se *)
  val pure0 : t (* c_se *)
  val pure1 : t

  val is_0pure : t -> bool
  val is_1pure : t -> bool
  val is_npure : int -> t -> bool

  val cons : e -> t -> t
  val tl : t -> t
  val hd : t -> e
  val chd : e -> t -> t
  val zip : t -> t -> t

  val of_int : int -> t
end

module SE_func (E:Effect) : SE_t = struct
  type t = E.e list [@@deriving ord]

  include E [@@deriving ord]

  let of_eff e = [e]

  let not_pure = [n_pure]
  let pure0 = [E.pure]
  let pure1 = [E.pure;E.pure]

  let cons a s = a::s
  let tl = function
    | [] -> []
    | _::s -> s
  let hd = function
    | [] -> E.n_pure
    | x::_ -> x
  let chd a = function
    | [] -> []
    | b::s -> (a @& b)::s
  let rec zip s1 s2 = match s1,s2 with
    | b1::l1, b2::l2 -> (b1@&b2)::(zip l1 l2)
    | _,[] | [],_ -> []

  let rec is_npure i s = match i with
    | 0 -> E.is_pure (hd s)
    | x when  x > 0 -> tl s |> is_npure (i-1)
    | _ -> false (* x < 0 *)
  let is_0pure = is_npure 0
  let is_1pure = is_npure 1

  let of_int = function (* for parser purposes *)
    | x when x >= 0 -> List.init (x+1) (fun _ -> E.pure)
    | _ -> not_pure
end

module SE = SE_func(Ebool)

type se = SE.t [@@deriving ord]
type st_env = se VarMap.t (* var -> stable *)

module PureEnv = Map.Make(String)
type penv = SE.t PureEnv.t

type const =
  | Unit | Nil
  | EmptyRecord
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Atom of string
[@@deriving show, ord]

type projection = Fst | Snd | Field of string
[@@deriving show, ord]

type 'typ type_annot = Unnanoted | ADomain of 'typ list
[@@deriving show, ord]

type ('a, 'typ, 'v) pattern =
  | PatType of 'typ
  | PatVar of 'v
  | PatAnd of ('a, 'typ, 'v) pattern * ('a, 'typ, 'v) pattern
  | PatOr of ('a, 'typ, 'v) pattern * ('a, 'typ, 'v) pattern
  | PatPair of ('a, 'typ, 'v) pattern * ('a, 'typ, 'v) pattern
  | PatRecord of (string * (('a, 'typ, 'v) pattern)) list * bool
  | PatAssign of 'v * const
[@@deriving ord]

and ('a, 'typ, 'v) ast =
  | Abstract of 'typ * se
  | Const of const
  | Var of 'v
  | Lambda of ('typ type_annot) * 'v * ('a, 'typ, 'v) t
  | Fixpoint of ('a, 'typ, 'v) t
  | Ite of ('a, 'typ, 'v) t * 'typ * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
  | App of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
  | Let of 'v * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
  | Pair of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
  | Projection of projection * ('a, 'typ, 'v) t
  | RecordUpdate of ('a, 'typ, 'v) t * string * ('a, 'typ, 'v) t option
  | Ref of ('a, 'typ, 'v) t
  | Read of ('a, 'typ, 'v) t
  | Assign of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
  | TypeConstr of ('a, 'typ, 'v) t * 'typ
  | PatMatch of ('a, 'typ, 'v) t
                * (('a, 'typ, 'v) pattern * ('a, 'typ, 'v) t) list
[@@deriving ord]

and ('a, 'typ, 'v) t = 'a * ('a, 'typ, 'v) ast

type parser_expr  = (annotation     , type_expr, varname   ) t
(* type se_expr      = (annotation * se, type_expr, varname   ) t *)
type annot_expr   = (annotation * se, typ      , Variable.t) t
type expr         = (unit       * se, typ      , Variable.t) t

module Expr = struct
  type el = expr
  type ord = Unknown | Lower | Equal | Greater
  let compare t1 t2 =
    let cstruct = compare
                    (fun ((),_) ((),_) -> 0)
                    (fun _ _ -> 0)
                    Variable.compare t1 t2
    in match cstruct with
       | -1 -> Lower
       | 1 -> Greater
       | 0 ->
          begin try
              let cexact =
                compare (fun ((),_) ((),_) -> 0)
                  Types.Compare.compare_typ
                  Variable.compare t1 t2 in
              match cexact with
              | -1 -> Lower
              | 1 -> Greater
              | 0 -> Equal
              | _ -> assert false
            with Types.Compare.Uncomparable -> Unknown
          end
       | _ -> assert false
end
module ExprMap = Pomap_impl.Make(Expr)

type name_var_map = Variable.t StrMap.t

let empty_name_var_map = StrMap.empty

let unique_exprid =
  let last_id = ref 0 in
  fun _ -> (
    last_id := !last_id + 1 ;
    !last_id
  )
let identifier_of_expr (a,_) = Position.value a
let position_of_expr (a,_) = Position.position a
let position_of_se_expr ((a,_),_) = Position.position a

let se_of ((_,s),_) = s
let is_npure i e = se_of e |> SE.is_npure i
let is_0pure e = se_of e |> SE.is_0pure
let is_1pure e = se_of e |> SE.is_1pure

let parser_expr_to_se_expr (penv:penv) e =
  let rec aux (penv:penv) (a,e) =
    let open SE in
    let args aux penv e =
      let e = aux penv e in
      let s = se_of e in
      (s,e)
    in
    let se,e = match e with
      | Abstract (t,p) -> p, Abstract (t,p)
      | Const c -> pure0, Const c
      | Var v ->
         (match PureEnv.find_opt v penv with
          | Some se -> se
          | None -> Format.printf "@{<yellow>@{<bold>Warning:@} variable %s \
                                   not in penv!@}" v;
                    pure0 )
        , Var v
      | Lambda (t, v, e) ->
         let penv = PureEnv.add v SE.pure0 penv in
         let s,e = args aux penv e in
         ( cons SE.pure s
         , Lambda (t, v, e) )
      | Fixpoint e ->
         let e = aux penv e in
         ( se_of e
         , Fixpoint e )
      | Ite (e, t, e1, e2) ->
         let s0, e  = args aux penv e  in
         let s1, e1 = args aux penv e1 in
         let s2, e2 = args aux penv e2 in
         ( chd (hd s0) (zip s1 s2)
         , Ite (e, t, e1, e2) )
      | App (e1, e2) ->
         let s1, e1 = args aux penv e1 in
         let s2, e2 = args aux penv e2 in
         ( chd SE.((hd s1) @& (hd s2)) (tl s1)
         , App (e1, e2) )
      | Let (v, e1, e2) ->
         let s1, e1 = args aux penv e1 in
         let penv = PureEnv.add v (cons pure (tl s1)) penv in
         let s2,e2 = args aux penv e2 in
         ( zip s1 s2
         , Let (v, e1, e2) )
      | Pair (e1, e2)  ->
         let s1, e1 = args aux penv e1 in
         let s2, e2 = args aux penv e2 in
         ( zip s1 s2
         , Pair (e1, e2) )
      | Projection (p, e) ->
         let e = aux penv e in
         ( se_of e
         , Projection (p, e))
      | RecordUpdate (e1, l, e2)  ->
         let s1, e1 = args aux penv e1 in
         let s2, e2 = match e2 with
           | None -> pure0 ,None
           | Some e2 -> let s,c = args aux penv e2 in s,(Some c)
         in
         ( zip s1 s2
         , RecordUpdate (e1, l, e2) )
      | Ref e -> (SE.(of_eff allocate), Ref (aux penv e))
      | Read e -> (SE.(of_eff read), Read (aux penv e))
      | Assign (e1,e2) -> (SE.(of_eff write), Assign (aux penv e1, aux penv e2))
      | TypeConstr (e,t) ->
         let e = aux penv e in
         ( se_of e
         , TypeConstr (e,t) )
      | PatMatch (e,pats) ->
         let s, e = args aux penv e in
         let pats = List.map (aux_pat penv) pats in
         let s' = List.fold_left (fun s (_,e) ->
                      se_of e |> zip s
                    ) SE.pure0 pats
         in
         ( zip s s'
         , PatMatch (e,pats) )
    in
    ((a,se),e)
  and aux_pat penv (pat,e) =
    let vars_in =
      let rec loop acc = function
        | PatType _ -> acc
        | PatVar v -> v::acc
        | PatAnd (p1, p2) | PatOr (p1,p2) | PatPair (p1,p2)
          -> let l1 = loop acc p1 in
             loop l1 p2
        | PatRecord (pl, _) ->
           List.fold_left (fun a (_,pat) -> loop a pat) acc pl
        | PatAssign (v, _) -> v::acc
      in
      loop []
    in
    let penv = vars_in pat
               |> List.fold_left (fun a v -> PureEnv.add v SE.pure0 a) penv in
    ( (pat :> ('a * se, 'b, varname) pattern) (* 'a not used in patterns ! *)
    , aux penv e)
  in
  aux penv e

let new_annot p =
  Position.with_pos p (unique_exprid ())

let copy_annot a =
  new_annot (Position.position a)

let dummy_pat_var_str = "_"
let dummy_pat_var =
  Variable.create_other (Some dummy_pat_var_str)

let se_expr_to_annot_expr tenv vtenv name_var_map e =
  let rec aux vtenv env (((exprid,pos),se),e) =
    let e = match e with
      | Abstract (t,p) ->
         let (t, _) = type_expr_to_typ tenv vtenv t in
         Abstract (t,p)
      | Const c -> Const c
      | Var str ->
         if StrMap.mem str env
         then Var (StrMap.find str env)
         else if has_atom tenv str
         then Const (Atom str)
         else raise (SymbolError ("undefined symbol "^str))
      | Lambda (t,str,e) ->
         let (t, vtenv) = match t with
           | Unnanoted -> (Unnanoted, vtenv)
           | ADomain ts ->
              let vtenv = ref vtenv in
              let ts = List.map (fun t ->
                           let (t, vtenv') = type_expr_to_typ tenv !vtenv t in
                           vtenv := vtenv' ; t
                         ) ts in
              (ADomain (ts), !vtenv)
         in
         let var = Variable.create_lambda (Some str) in
         Variable.attach_location var pos ;
         let env = StrMap.add str var env in
         Lambda (t, var, aux vtenv env e)
      | Fixpoint e -> Fixpoint (aux vtenv env e)
      | Ite (e, t, e1, e2) ->
         let (t, vtenv) = type_expr_to_typ tenv vtenv t in
         if is_test_type t
         then Ite (aux vtenv env e, t, aux vtenv env e1, aux vtenv env e2)
         else raise (SymbolError ("typecases must use a valid test type"))
      | App (e1, e2) -> App (aux vtenv env e1, aux vtenv env e2)
      | Let (str, e1, e2) ->
         let var = Variable.create_other (Some str) in
         Variable.attach_location var pos ;
         let env' = StrMap.add str var env in
         Let (var, aux vtenv env e1, aux vtenv env' e2)
      | Pair (e1, e2) ->
         Pair (aux vtenv env e1, aux vtenv env e2)
      | Projection (p, e) -> Projection (p, aux vtenv env e)
      | RecordUpdate (e1, l, e2) ->
         RecordUpdate (aux vtenv env e1, l, Option.map (aux vtenv env) e2)
      | Ref e ->
         let ref_t = ((exprid,pos),se)
                   , Var (StrMap.find Variable.ref_create env) in
         App (ref_t, aux vtenv env e)
      | Read e ->
         let get_t = ((exprid,pos),se)
                   , Var (StrMap.find Variable.ref_get env) in
         App (get_t, aux vtenv env e)
      | Assign (e1,e2) ->
         let set_t = ((exprid,pos),se)
                   , Var (StrMap.find Variable.ref_set env) in
         let tmp_t = ((exprid,pos),se)
                   , App (set_t, aux vtenv env e1) in
         App (tmp_t, aux vtenv env e2)
      | TypeConstr (e, t) ->
         let (t, vtenv) = type_expr_to_typ tenv vtenv t in
         if is_test_type t
         then TypeConstr (aux vtenv env e, t)
         else raise (SymbolError
                       ("type constraints must be a valid test type"))
      | PatMatch (e, pats) ->
         PatMatch (aux vtenv env e, List.map (aux_pat pos vtenv env) pats)
    in
    (((exprid,pos),se),e)
  and aux_pat pos vtenv env (pat, e) =
    let merge_disj =
      StrMap.union (fun str v1 v2 ->
          if Variable.equals v1 v2 then Some v1
          else raise (SymbolError
                        ("matched variables "^str^" are conflicting")))
    in
    (* let expr_env = env in *)
    let rec aux_p vtenv env pat =
      let find_or_def_var str =
        if StrMap.mem str env
        then StrMap.find str env
        else
          let var = Variable.create_other (Some str) in
          Variable.attach_location var pos ;
          var
      in
      match pat with
      | PatType t ->
         let (t, vtenv) = type_expr_to_typ tenv vtenv t in
         if is_test_type t
         then (PatType t, vtenv, StrMap.empty)
         else raise (SymbolError ("typecases must use a valid test type"))
      | PatVar str ->
         if String.equal str dummy_pat_var_str
         then (PatVar dummy_pat_var, vtenv, StrMap.empty)
         else
           let var = find_or_def_var str in
           (PatVar var, vtenv, StrMap.singleton str var)
      | PatAnd (p1, p2) ->
         let (p1, vtenv, env1) = aux_p vtenv env p1 in
         let (p2, vtenv, env2) = aux_p vtenv env p2 in
         (PatAnd (p1, p2), vtenv, merge_disj env1 env2)
      | PatOr (p1, p2) ->
         let (p1, vtenv, env1) = aux_p vtenv env p1 in
         let env = merge_disj env env1 in
         let (p2, vtenv, env2) = aux_p vtenv env p2 in
         if StrMap.equal (Variable.equals) env1 env2 |> not
         then raise (SymbolError ("missing matched variables in pattern")) ;
         (PatOr (p1, p2), vtenv, env1)
      | PatPair (p1, p2) ->
         let (p1, vtenv, env1) = aux_p vtenv env p1 in
         let (p2, vtenv, env2) = aux_p vtenv env p2 in
         (PatPair (p1, p2), vtenv, merge_disj env1 env2)
      | PatRecord (fields, o) ->
         let (fields, vtenv, env) =
           List.fold_left
             (fun (fields, vtenv, acc_env) (name, p) ->
               let (p, vtenv, env') = aux_p vtenv env p in
               ((name, p)::fields, vtenv, merge_disj acc_env env')
             ) ([], vtenv, env) fields in
         (PatRecord (List.rev fields, o), vtenv, env)
      | PatAssign (str, c) ->
         if String.equal str dummy_pat_var_str
         then raise (SymbolError
                       "invalid variable name for a pattern assignement") ;
         let var = find_or_def_var str in
         (* let e = aux vtenv expr_env e in
            (PatAssign (var, e), vtenv, StrMap.singleton str var) *)
         (PatAssign (var, c), vtenv, StrMap.singleton str var)
    in
    let (pat, vtenv, env') = aux_p vtenv StrMap.empty pat in
    let env = StrMap.add_seq (StrMap.to_seq env') env in
    (pat, aux vtenv env e)
  in
  aux vtenv name_var_map e

let parser_expr_to_annot_expr tenv vtenv name_var_map penv e =
  parser_expr_to_se_expr penv e
  |> se_expr_to_annot_expr tenv vtenv name_var_map

let map_p f p =
  let rec aux p =
    let p =
      match p with
      | PatAssign (v, e) -> PatAssign (v, e)
      | PatType t -> PatType t
      | PatVar v -> PatVar v
      | PatAnd (p1, p2) -> PatAnd (aux p1, aux p2)
      | PatOr (p1, p2) -> PatOr (aux p1, aux p2)
      | PatPair (p1, p2) -> PatPair (aux p1, aux p2)
      | PatRecord (fields, o) ->
         PatRecord (List.map (fun (str, p) -> (str, aux p)) fields, o)
    in
    f p
  in
  aux p

let rec unannot ((_,se),e) =
  let e = match e with
    | Abstract (t,p) -> Abstract (t,p)
    | Const c -> Const c
    | Var v -> Var v
    | Lambda (t, v, e) -> Lambda (t, v, unannot e)
    | Fixpoint e -> Fixpoint (unannot e)
    | Ite (e, t, e1, e2) -> Ite (unannot e, t, unannot e1, unannot e2)
    | App (e1, e2) -> App (unannot e1, unannot e2)
    | Let (v, e1, e2) -> Let (v, unannot e1, unannot e2)
    | Pair (e1, e2) -> Pair (unannot e1, unannot e2)
    | Projection (p, e) -> Projection (p, unannot e)
    | RecordUpdate (e1, l, e2) ->
       RecordUpdate (unannot e1, l, Option.map unannot e2)
    | Ref _ | Read _ | Assign _ -> assert false
    | TypeConstr (e, t) -> TypeConstr (unannot e, t)
    | PatMatch (e, pats) ->
       PatMatch (unannot e
                ,pats |> List.map (fun (p, e) -> (unannot_pat p, unannot e)))
  in
  ( ((),se), e )

and unannot_pat pat =
  let rec aux pat =
    match pat with
    | PatAssign (v, c) -> PatAssign (v, c (* unannot e *))
    | PatType t -> PatType t
    | PatVar v -> PatVar v
    | PatAnd (p1, p2) -> PatAnd (aux p1, aux p2)
    | PatOr (p1, p2) -> PatOr (aux p1, aux p2)
    | PatPair (p1, p2) -> PatPair (aux p1, aux p2)
    | PatRecord (fields, o) ->
       PatRecord (List.map (fun (str, p) -> (str, aux p)) fields, o)
  in
  aux pat

let predefined_vars = Hashtbl.create 100
let get_predefined_var i =
  if Hashtbl.mem predefined_vars i
  then Hashtbl.find predefined_vars i
  else
    let v = Variable.create_other None in
    Hashtbl.add predefined_vars i v ;
    v
let normalize_bvs e =
  let rec aux depth map (a, e) =
    let e = match e with
      | Abstract (t,p) -> Abstract (t,p)
      | Const c -> Const c
      | Var v when VarMap.mem v map -> Var (VarMap.find v map)
      | Var v -> Var v
      | Lambda (t, v, e) ->
         let v' = get_predefined_var depth in
         let map = VarMap.add v v' map in
         Lambda (t, v', aux (depth+1) map e)
      | Fixpoint e -> Fixpoint (aux depth map e)
      | Ite (e, t, e1, e2) ->
         Ite (aux depth map e, t, aux depth map e1, aux depth map e2)
      | App (e1, e2) ->
         App (aux depth map e1, aux depth map e2)
      | Let (v, e1, e2) ->
         let e1 = aux depth map e1 in
         let v' = get_predefined_var depth in
         let map = VarMap.add v v' map in
         Let (v', e1, aux (depth+1) map e2)
      | Pair (e1, e2) ->
         Pair (aux depth map e1, aux depth map e2)
      | Projection (p, e) -> Projection (p, aux depth map e)
      | RecordUpdate (e1, l, e2) ->
         RecordUpdate (aux depth map e1, l, Option.map (aux depth map) e2)
      | Ref _ | Read _ | Assign _ -> assert false
      | TypeConstr (e, t) -> TypeConstr (aux depth map e, t)
      | PatMatch (e, pats) ->
         let e = aux depth map e in
         (* NOTE: We do not normalize pattern variables,
            as two pattern matchings will almost never be
            syntactically equivalent anyway. *)
         let pats = pats |> List.map (fun (p,e) ->
                                (aux_p depth map p, aux depth map e)) in
         PatMatch (e, pats)
    in (a, e)
  and aux_p (*depth map*) _ _ pat =
    let pa pat =
      match pat with
      | PatAssign (v, c) -> PatAssign (v, c (* aux depth map e *))
      | p -> p
    in
    map_p pa pat
  in aux 0 VarMap.empty e

let unannot_and_normalize e = e |> unannot |> normalize_bvs

(*let rec fv (_, expr) =
  match expr with
  | Abstract _ | Const _ -> VarSet.empty
  | Var v -> VarSet.singleton v
  | Lambda (_, v, e) -> VarSet.remove v (fv e)
  | Ite (e, _, e1, e2) -> VarSet.union (VarSet.union (fv e) (fv e1)) (fv e2)
  | App (e1, e2) -> VarSet.union (fv e1) (fv e2)
  | Let (v, e1, e2) -> VarSet.union (fv e1) (VarSet.remove v (fv e2))
  | Pair (e1, e2) -> VarSet.union (fv e1) (fv e2)
  | Projection (_, e) -> fv e
  | RecordUpdate (e1, _, e2) ->
     begin match e2 with
     | Some e2 -> VarSet.union (fv e1) (fv e2)
     | None -> fv e1
     end
  | Debug (_, e) -> fv e*)

let map_ast f e =
  let rec aux (annot, e) =
    let e = match e with
      | Abstract (t,p) -> Abstract (t,p)
      | Const c -> Const c
      | Var v -> Var v
      | Lambda (annot, v, e) -> Lambda (annot, v, aux e)
      | Fixpoint e -> Fixpoint (aux e)
      | Ite (e, t, e1, e2) -> Ite (aux e, t, aux e1, aux e2)
      | App (e1, e2) -> App (aux e1, aux e2)
      | Let (v, e1, e2) -> Let (v, aux e1, aux e2)
      | Pair (e1, e2) -> Pair (aux e1, aux e2)
      | Projection (p, e) -> Projection (p, aux e)
      | RecordUpdate (e, str, eo) ->
         RecordUpdate (aux e, str, Option.map aux eo)
      | Ref e -> Ref (aux e)
      | Read e -> Read (aux e)
      | Assign (e1, e2) -> Assign (aux e1, aux e2)
      | TypeConstr (e, t) -> TypeConstr (aux e, t)
      | PatMatch (e, pats) ->
         let pats = pats |> List.map (fun (p,e) -> (aux_p p, aux e)) in
         PatMatch (aux e, pats)
    in
    f (annot, e)
  and aux_p p =
    let pa p =
      match p with
      | PatAssign (v, c) -> PatAssign (v, c (* aux e *))
      | p -> p
    in
    map_p pa p
  in
  aux e

let substitute aexpr v (annot', expr') =
  let aux (_, expr) =
    let expr = match expr with
      | Var v' when Variable.equals v v' -> expr'
      | Lambda (ta, v', e) ->
         assert (Variable.equals v v' |> not) ;
         Lambda (ta, v', e)
      | Let (v', e1, e2) ->
         assert (Variable.equals v v' |> not) ;
         Let (v', e1, e2)
      | e -> e
    in
    (annot', expr)
  in map_ast aux aexpr

let const_to_typ c =
  match c with
  | Unit -> unit_typ
  | Nil -> nil_typ
  | EmptyRecord -> empty_closed_record
  | Bool true -> true_typ
  | Bool false -> false_typ
  | Int i -> interval (Some i) (Some i)
  | Float _ -> float_typ
  | Char c -> single_char c
  | String str -> single_string str
  | Atom t -> raise (SymbolError ("undefined atom "^t))

type parser_element =
  | Definition of (int * (string * parser_expr * type_expr option))
  | Atoms of string list
  | Types of (string * string list * type_expr) list

type parser_program = (annotation * parser_element) list
