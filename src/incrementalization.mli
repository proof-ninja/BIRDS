
open Expr

type error

val string_of_error : error -> string

val incrementalization_ast : expr -> (rule list, error) result
