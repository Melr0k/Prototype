open Types.Base
open Types.Additions
open Variable
open Pomap

exception UndefinedSymbol of string

exception LexicalError of int * string
exception SyntaxError of string * string (* position * msg *)

type varname = string
type exprid = int


type annotation = exprid Position.located

type const =
| Unit | Nil
| EmptyRecord
| Bool of bool
| Int of int
| Char of char
| String of string
| Atom of string
[@@deriving show, ord]

type projection = Fst | Snd | Field of string
[@@deriving show, ord]

type 'typ type_annot = Unnanoted | ADomain of 'typ list | AArrow of 'typ
[@@deriving show, ord]

type ('a, 'typ, 'v) ast =
| Abstract of 'typ
| Const of const
| Var of 'v
| Lambda of ('typ type_annot) * 'v * ('a, 'typ, 'v) t
| Ite of ('a, 'typ, 'v) t * 'typ * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| App of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Let of 'v * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Pair of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Projection of projection * ('a, 'typ, 'v) t
| RecordUpdate of ('a, 'typ, 'v) t * string * ('a, 'typ, 'v) t option
[@@deriving ord]

and ('a, 'typ, 'v) t = 'a * ('a, 'typ, 'v) ast

type annot_expr = (annotation, typ, Variable.t) t
type expr = (unit, typ, Variable.t) t
type parser_expr = (annotation, type_expr, varname) t

module Expr = struct
    type el = expr
    type ord = Unknown | Lower | Equal | Greater
    let compare t1 t2 =
        let cstruct = compare
            (fun () () -> 0)
            (fun _ _ -> 0)
            Variable.compare t1 t2
        in match cstruct with
        | -1 -> Lower
        | 1 -> Greater
        | 0 ->
            begin try
                let cexact = compare
                    (fun () () -> 0)
                    Types.Compare.compare_typ
                    Variable.compare t1 t2 in
                match cexact with
                | -1 -> Lower
                | 1 -> Greater
                | 0 -> Equal
                | _ -> assert false
            with Types.Compare.Uncomparable -> Unknown end
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

let new_annot p =
    Position.with_pos p (unique_exprid ())

let copy_annot a =
    new_annot (Position.position a)

let parser_expr_to_annot_expr tenv vtenv name_var_map e =
    let rec aux vtenv env ((exprid,pos),e) =
        let e = match e with
        | Abstract t ->
            let (t, _) = type_expr_to_typ tenv vtenv t in
            Abstract t
        | Const c -> Const c
        | Var str ->
            if StrMap.mem str env
            then Var (StrMap.find str env)
            else if has_atom tenv str
            then Const (Atom str)
            else raise (UndefinedSymbol (str))
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
            | AArrow t ->
                let (t, vtenv) = type_expr_to_typ tenv vtenv t in
                (AArrow (t), vtenv)
            in
            let var = Variable.create (Some str) in
            Variable.attach_location var pos ;
            let env = StrMap.add str var env in
            Lambda (t, var, aux vtenv env e)
        | Ite (e, t, e1, e2) ->
            let (t, vtenv) = type_expr_to_typ tenv vtenv t in
            if is_test_type t
            then Ite (aux vtenv env e, t, aux vtenv env e1, aux vtenv env e2)
            else failwith "Cannot make a typecase with a non-trvial arrow type."
        | App (e1, e2) ->
            App (aux vtenv env e1, aux vtenv env e2)
        | Let (str, e1, e2) ->
            let var = Variable.create (Some str) in
            Variable.attach_location var pos ;
            let env' = StrMap.add str var env in
            Let (var, aux vtenv env e1, aux vtenv env' e2)
        | Pair (e1, e2) ->
            Pair (aux vtenv env e1, aux vtenv env e2)
        | Projection (p, e) -> Projection (p, aux vtenv env e)
        | RecordUpdate (e1, l, e2) ->
            RecordUpdate (aux vtenv env e1, l, Option.map (aux vtenv env) e2)
        in
        ((exprid,pos),e)
    in
    aux vtenv name_var_map e

let rec unannot (_,e) =
    let e = match e with
    | Abstract t -> Abstract t
    | Const c -> Const c
    | Var v -> Var v
    | Lambda (t, v, e) -> Lambda (t, v, unannot e)
    | Ite (e, t, e1, e2) -> Ite (unannot e, t, unannot e1, unannot e2)
    | App (e1, e2) -> App (unannot e1, unannot e2)
    | Let (v, e1, e2) -> Let (v, unannot e1, unannot e2)
    | Pair (e1, e2) -> Pair (unannot e1, unannot e2)
    | Projection (p, e) -> Projection (p, unannot e)
    | RecordUpdate (e1, l, e2) ->
        RecordUpdate (unannot e1, l, Option.map unannot e2)
    in
    ( (), e )

let normalize_bvs e =
    let rec aux depth map (a, e) =
        let e = match e with
        | Abstract t -> Abstract t
        | Const c -> Const c
        | Var v when VarMap.mem v map -> Var (VarMap.find v map)
        | Var v -> Var v
        | Lambda (t, v, e) ->
            let v' = get_predefined_var depth in
            let map = VarMap.add v v' map in
            Lambda (t, v', aux (depth+1) map e)
        | Ite (e, t, e1, e2) ->
            Ite (aux depth map e, t, aux depth map e1, aux depth map e2)
        | App (e1, e2) ->
            App (aux depth map e1, aux depth map e2)
        | Let (v, e1, e2) ->
            let v' = get_predefined_var depth in
            let map = VarMap.add v v' map in
            Let (v', aux (depth+1) map e1, aux (depth+1) map e2)
        | Pair (e1, e2) ->
            Pair (aux depth map e1, aux depth map e2)
        | Projection (p, e) -> Projection (p, aux depth map e)
        | RecordUpdate (e1, l, e2) ->
            RecordUpdate (aux depth map e1, l, Option.map (aux depth map) e2)
        in (a, e)
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

let substitute aexpr v (annot', expr') =
  let rec aux (_, expr) =
    let expr = match expr with
    | Abstract t -> Abstract t
    | Const c -> Const c
    | Var v' when Variable.equals v v' -> expr'
    | Var v' -> Var v'
    | Lambda (ta, v', e) when Variable.equals v v' -> Lambda (ta, v', e)
    | Lambda (ta, v', e) -> Lambda (ta, v', aux e)
    | Ite (e, t, e1, e2) -> Ite (aux e, t, aux e1, aux e2)
    | App (e1, e2) -> App (aux e1, aux e2)
    | Let (v', e1, e2) when Variable.equals v v' -> Let (v', aux e1, e2)
    | Let (v', e1, e2) -> Let (v', aux e1, aux e2)
    | Pair (e1, e2) -> Pair (aux e1, aux e2)
    | Projection (p, e) -> Projection (p, aux e)
    | RecordUpdate (e1, f, e2) ->
      let e2 = match e2 with
      | Some e2 -> Some (aux e2)
      | None -> None
      in RecordUpdate (aux e1, f, e2)
    in
    (annot', expr)
  in aux aexpr

let const_to_typ c =
    match c with
    | Unit -> unit_typ
    | Nil -> nil_typ
    | EmptyRecord -> empty_closed_record
    | Bool true -> true_typ
    | Bool false -> false_typ
    | Int i -> interval (Some i) (Some i)
    | Char c -> single_char c
    | String str -> single_string str
    | Atom t ->
        failwith (Printf.sprintf "Can't retrieve the type of the atom %s." t)

type parser_element =
| Definition of (int * (string * parser_expr))
| Atoms of string list
| Types of (string * type_expr) list

type parser_program = parser_element list
