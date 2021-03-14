(*open Variable*)

val partition : Cduce.typ -> Cduce.typ list -> Cduce.typ list

module VarAnnot : sig
  type t
  val empty : t
  val any : t
  val is_empty : t -> bool
  val singleton : Env.t -> Cduce.typ -> t
  val splits : Env.t -> t -> Cduce.typ list
  val add_split : Env.t -> Cduce.typ -> t -> t
  val cup : t -> t -> t
  val union : t list -> t
  val pp_filtered : string list -> Format.formatter -> t -> unit
  val pp : Format.formatter -> t -> unit
end

(*module Annotations : sig
  type t
  val empty : t
  val is_undefined : Variable.t -> t -> bool
  val is_empty : Variable.t -> t -> bool
  val splits : Variable.t -> Env.t -> t -> Cduce.typ list
  val add_split : Variable.t -> Env.t -> Cduce.typ -> t -> t
  val cup : t -> t -> t
  val union : t list -> t

  val mem_var : Variable.t -> t -> bool
  val add_var : Variable.t -> VarAnnot.t -> t -> t
  val remove_var : Variable.t -> t -> t
  val get_var : Variable.t -> t -> VarAnnot.t
  val restrict : VarSet.t -> t -> t
end*)
