open Common

open Types_additions
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
[@@deriving ord]

type projection = Fst | Snd | Field of string
[@@deriving ord]

type 'typ type_annot = Unnanoted | ADomain of 'typ | AArrow of 'typ
[@@deriving ord]

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
  | Ref of ('a, 'typ, 'v) t
  | Read of ('a, 'typ, 'v) t
  | Assign of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
(*
  | Seq of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
 *)
[@@deriving ord]

and ('a, 'typ, 'v) t = 'a * ('a, 'typ, 'v) ast

type se = bool
type st_env = se VarMap.t

type parser_expr = (annotation     , type_expr, varname   ) t
type annot_expr  = (annotation * se, Cduce.typ, Variable.t) t
type expr        = (             se, Cduce.typ, Variable.t) t

module Expr = struct
  type el = expr
  type ord = Unknown | Lower | Equal | Greater
  let compare t1 t2 =
    try (
      let i = compare
                (fun _  _  -> 0)
                Types_compare.compare_typ
                Variable.compare t1 t2 in
      match i with
      | 0 -> Equal
      | -1 -> Lower
      | 1 -> Greater
      | _ -> assert false
    )
    with Types_compare.Uncomparable -> Unknown
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

let annotate tenv vtenv name_var_map e =
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
         else raise (UndefinedSymbol str)
      | Lambda (t,str,e) ->
         let (t, vtenv) = match t with
           | Unnanoted -> (Unnanoted, vtenv)
           | ADomain t ->
              let (t, vtenv) = type_expr_to_typ tenv vtenv t in
              (ADomain t, vtenv)
           | AArrow t ->
              let (t, vtenv) = type_expr_to_typ tenv vtenv t in
              (AArrow t, vtenv)
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
         RecordUpdate (aux vtenv env e1, l, Utils.option_map (aux vtenv env) e2)
      | Ref e -> Ref (aux vtenv env e)               (*   TODO    *)
      | Read e -> Read (aux vtenv env e)             (*  verify   *)
      | Assign (e1,e2) -> Assign (aux vtenv env e1,  (* this code *)
                                  aux vtenv env e2)
    in
    ((exprid,pos),e)
  in
  aux vtenv name_var_map e

let st stenv ae =
  let rec aux stenv (a, e) =
    let se, e = match e with
      | Abstract t -> true, Abstract t
      | Const c -> true, Const c
      | Var v -> begin match VarMap.find_opt v stenv with
                 | Some s -> s, Var v
                 | None -> failwith "Variable not found in st environnement."
                 end
      | Lambda (t, v, e) ->
         let (_,se), _ as e = aux (VarMap.add v true stenv) e in
         se, Lambda (t, v, e)
      | Ite (e, t, e1, e2) ->
         let (_,s ), _ as e  = aux stenv e  in
         let (_,s1), _ as e1 = aux stenv e1 in
         let (_,s2), _ as e2 = aux stenv e2 in
         s && s1 && s2, Ite (e, t, e1, e2)
      | App (e1, e2) ->
         let (_,s1), _ as e1 = aux stenv e1 in
         let (_,s2), _ as e2 = aux stenv e2 in
         s1 && s2,  App (e1, e2)
      | Let (v, e1, e2) ->
         let (_,s1), _ as e1 = aux stenv e1 in
         let (_,s2), _ as e2 = aux (VarMap.add v s1 stenv) e2 in
         s1 && s2, Let (v, e1, e2)
      | Pair (e1, e2) ->
         let (_,s1), _ as e1 = aux stenv e1 in
         let (_,s2), _ as e2 = aux stenv e2 in
         s1 && s2, Pair (e1, e2)
      | Projection (p, e) ->
         let (_,s), _ as e = aux stenv e in
         s, Projection (p, e)
      | RecordUpdate (e1, l, e2) ->
         let (_,s1), _ as e1 = aux stenv e1 in
         let s2, e2 = begin match Utils.option_map (aux stenv) e2 with
                      | None -> true, None
                      | Some ((_,s), _) as e -> s, e
                      end in
         s1 && s2, RecordUpdate (e1, l, e2)
      | Ref e ->
         let (_,s), _ as e = aux stenv e in
         s, Ref e
      | Read e -> false, Read (aux stenv e)
      | Assign (e1, e2) ->
         let (_,s1), _ as e1 = aux stenv e1 in
         let (_,s2), _ as e2 = aux stenv e2 in
         s1 && s2, Assign (e1, e2)
    in
    ((a,se), e)
  in
  aux stenv ae

let is_st se = se
let is_st_annot ((_,se),_) = is_st se
and is_st_expr (se,_) = is_st se

let parser_expr_to_annot_expr tenv vtenv name_var_map stenv e =
  annotate tenv vtenv name_var_map e |> st stenv

let rec unannot ((_,b),e) =
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
       RecordUpdate (unannot e1, l, Utils.option_map unannot e2)
    | Ref e -> Ref (unannot e)
    | Read e -> Read (unannot e)
    | Assign (e1, e2) -> Assign (unannot e1, unannot e2)
    in
    ( b, e )

let normalize_bvs e =
  let rec aux depth map (stable, e) =
    let e = match e with
      | Abstract t -> Abstract t
      | Const c -> Const c
      | Var v when stable && VarMap.mem v map ->
         Var (VarMap.find v map)
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
         (*            ↓  why both depth+1 ? ↓             *)
         Let (v', aux (depth+1) map e1, aux (depth+1) map e2)
      | Pair (e1, e2) ->
         Pair (aux depth map e1, aux depth map e2)
      | Projection (p, e) -> Projection (p, aux depth map e)
      | RecordUpdate (e1, l, e2) ->
         RecordUpdate (aux depth map e1, l, Utils.option_map (aux depth map) e2)
      | Ref e -> Ref (aux depth map e)
      | Read e -> Ref (aux depth map e)
      | Assign (e1,e2) -> Assign (aux depth map e1, aux depth map e2)
    in (stable, e)
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

(** substitute v by (annot', expr') in aexpr **)
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
      | Ref e -> Ref (aux e)
      | Read e -> Read (aux e)
      | Assign (e1, e2) -> Assign (aux e1, aux e2)
    in
    (annot', expr)
  in aux aexpr

let const_to_typ c =
  match c with
  | Unit -> Cduce.unit_typ
  | Nil -> Cduce.nil_typ
  | EmptyRecord -> Cduce.empty_closed_record
  | Bool true -> Cduce.true_typ
  | Bool false -> Cduce.false_typ
  | Int i -> Cduce.interval (Some i) (Some i)
  | Char c -> Cduce.single_char c
  | String str -> Cduce.single_string str
  | Atom t ->
     failwith (Printf.sprintf "Can't retrieve the type of the atom %s." t)

type parser_element =
  | Definition of (bool * (string * parser_expr))
  | Atoms of string list
  | Types of (string * type_expr) list

type parser_program = parser_element list

(* Pretty-printers *)

let rec pp_const fmt = function
  | Unit        -> Format.fprintf fmt "()"
  | Nil         -> Format.fprintf fmt "`nil"
  | EmptyRecord -> Format.fprintf fmt "{}"
  | Bool b      -> Format.fprintf fmt (if b then "true" else "false")
  | Int i       -> Format.fprintf fmt "%d" i
  | Char c      -> Format.fprintf fmt "%c" c
  | String s    -> Format.fprintf fmt "%s" s
  | Atom a      -> Format.fprintf fmt "%s" a
and pp_projection fmt = function
  | Fst -> Format.fprintf fmt "fst"
  | Snd -> Format.fprintf fmt "snd"
  | Field s -> Format.fprintf fmt "%s" s
and pp_type_annot fa fmt = function
  | Unnanoted -> Format.fprintf fmt ""
  | ADomain t -> Format.fprintf fmt " <@[%a@]>" fa t
  | AArrow t  -> Format.fprintf fmt " <@[%a@]>" fa t
and pp_ast fmt_a fmt_typ fmt_v fmt = function
  | Abstract t -> Format.fprintf fmt "%a" fmt_typ t
  | Const c -> Format.fprintf fmt "%a" pp_const c
  | Var v -> Format.fprintf fmt "%a" fmt_v v
  | Lambda (ta, v, (_,e)) ->
     Format.fprintf fmt "@[<v 2>fun%a %a ->@ %a@]"
       (pp_type_annot fmt_typ) ta fmt_v v (pp_ast fmt_a fmt_typ fmt_v) e
  | Ite ((_,e), t, (_,e1), (_,e2)) ->
     Format.fprintf fmt "@[<hov>if@ %a@ is@ %a@\n\
                         then@[<hov 2>@ %a@]@\n\
                         else@[<hov 2>@ %a@]@]"
       (pp_ast fmt_a fmt_typ fmt_v) e fmt_typ t
       (pp_ast fmt_a fmt_typ fmt_v) e1 (pp_ast fmt_a fmt_typ fmt_v) e2
  | App ((_,e1), (_,e2)) ->
     Format.fprintf fmt "@[%a@ %a@]"
       (pp_ast fmt_a fmt_typ fmt_v) e1 (pp_ast fmt_a fmt_typ fmt_v) e2
  | Let (v, (_,e1), (_,e2)) ->
     Format.fprintf fmt "@[<hov 2>let %a =@ %a@ @]in@\n%a" fmt_v v
       (pp_ast fmt_a fmt_typ fmt_v) e1 (pp_ast fmt_a fmt_typ fmt_v) e2
  | Pair ((_,e1), (_,e2)) ->
     Format.fprintf fmt "(@[%a@],@ @[%a@])"
       (pp_ast fmt_a fmt_typ fmt_v) e1 (pp_ast fmt_a fmt_typ fmt_v) e2
  | Projection (p, (_,e)) ->
     Format.fprintf fmt "%a.%a"
       (pp_ast fmt_a fmt_typ fmt_v) e pp_projection p
  | RecordUpdate ((_,e1), l, None) ->
     Format.fprintf fmt "{%a; \ %s}"
       (pp_ast fmt_a fmt_typ fmt_v) e1 l
  | RecordUpdate ((_,e1), l, Some (_,e2)) ->
     Format.fprintf fmt "{%a; %s =@ %a}"
       (pp_ast fmt_a fmt_typ fmt_v) e1 l (pp_ast fmt_a fmt_typ fmt_v) e2
  | Ref (_,e) ->
     Format.fprintf fmt "@[<hov 2>ref@ %a@]" (pp_ast fmt_a fmt_typ fmt_v) e
  | Read (_,e) -> Format.fprintf fmt "!@[%a@]" (pp_ast fmt_a fmt_typ fmt_v) e
  | Assign ((_,e1), (_,e2)) ->
     Format.fprintf fmt "@[%a@] :=@ %a"
       (pp_ast fmt_a fmt_typ fmt_v) e1 (pp_ast fmt_a fmt_typ fmt_v) e2

let pp_varname fmt s = Format.fprintf fmt "%s" s
and pp_annotation fmt _ = Format.fprintf fmt "[opaque]"

let pp_parser_element fmt = function
  | Definition (_,(fid,(_,ast))) ->
     Format.fprintf fmt "@[<hov 2>Let %s =@ %a@];;"
       (*if b then "⊤" else "⊥"*) fid
       (pp_ast pp_annotation pp_type_expr pp_varname) ast
  | Atoms s -> List.fold_left
                 (fun () a -> Format.fprintf fmt "@ %s" a)
                 (Format.fprintf fmt "@[<hov 2>atoms :")
                 s;
               Format.fprintf fmt "@]@\n"
  | Types l -> List.fold_left
                 (fun () (s,t) -> Format.fprintf fmt "@ (%s :@ %a)"
                                    s Types_additions.pp_type_expr t)
                 (Format.fprintf fmt "@[<hov 2>types :")
                 l;
               Format.fprintf fmt "@]@\n"

let show_parser_program =
  List.fold_left
    (fun str e -> Format.asprintf "%s%a\n" str pp_parser_element e)
    ""
