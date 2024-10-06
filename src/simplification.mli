
open Expr

type error

val simplify : rule list -> view option -> source list -> (rule list, error) result

val string_of_error : error -> string
