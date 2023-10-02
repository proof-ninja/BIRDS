
open Expr

type error

val simplify : rule list -> (rule list, error) result

val string_of_error : error -> string
