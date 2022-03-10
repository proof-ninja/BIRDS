(*
val source_to_lean_func_types : Expr.expr -> string list
*)

type lean_theorem

val lean_simp_theorem_of_disjoint_delta : bool -> Expr.expr -> lean_theorem

val lean_simp_theorem_of_getput : bool -> Expr.expr -> lean_theorem

val lean_simp_theorem_of_putget : bool -> Expr.expr -> lean_theorem

val lean_theorem_of_view_existence : bool -> Expr.expr -> Fol.fol Formulas.formula -> Fol.fol Formulas.formula -> lean_theorem

val gen_lean_code_for_theorems : lean_theorem list -> string

val validity_lean_code_of_bidirectional_datalog : bool -> Expr.expr -> string
