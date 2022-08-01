open Msc
open Types.Base
open Types.Additions
open Common

exception Ill_typed of Position.t list * string

val typeof : legacy:bool -> type_env -> Env.t -> e -> typ
val typeof_a : legacy:bool -> Position.t list -> type_env -> Env.t -> a -> typ

val refine_a : sufficient:bool -> type_env -> Ref_env.t
               -> a -> typ -> typ -> Ref_env.t list

val infer_legacy : type_env -> Env.t -> e -> e
val typeof_simple_legacy : type_env -> Env.t -> e -> typ

val infer : type_env -> Env.t -> e -> e
val typeof_simple : type_env -> Env.t -> e -> typ
