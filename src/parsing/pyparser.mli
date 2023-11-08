open PyreAst
open Ast

val pos_of_loc : ?filename:string -> ?bol:int -> Concrete.Location.t
                 -> Position.t
val py_to_t : ?filename:string -> Concrete.Module.t -> parser_expr
val parse : string -> Concrete.Module.t
