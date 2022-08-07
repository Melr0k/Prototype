open Types.Base
open Common
open Parsing.Variable

val partition : typ list-> typ list
val regroup : ('a -> 'a -> bool) -> ('a * 'b) list -> ('a * ('b list)) list

module Refinements : sig
    type t = Env.t list
    val dom : t -> Variable.t list
    val project : t -> Variable.t -> typ list
    val partition : t -> t
    val compatibles : Env.t -> t -> t
end
