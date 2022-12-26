val folterm_of_vterm : Expr2.vterm -> Fol.term
val const_of_string : string -> Expr2.const
val vterm_of_folterm : Fol.term -> Expr2.vterm
val fol_of_symtkey :
  Utils.symtable ->
  Utils.colnamtab -> Utils.symtkey -> Fol.fol Formulas.formula
val fol_of_rule :
  Utils.symtable ->
  Utils.colnamtab ->
  Expr2.rterm * Expr2.term list -> Fol.fol Formulas.formula
val fol_of_rterm :
  Expr2.rterm ->
  Utils.symtable -> Utils.colnamtab -> Fol.fol Formulas.formula
val fol_or_eterm : Expr2.eterm -> Fol.fol Formulas.formula
val fol_of_eq : Expr2.term -> Fol.fol Formulas.formula
val fol_of_ineq : Expr2.term -> Fol.fol Formulas.formula
val fol_of_query :
  Utils.symtable ->
  Utils.colnamtab -> Expr2.rterm -> string list * Fol.fol Formulas.formula
val fol_of_stt : bool -> Expr2.expr -> string list * Fol.fol Formulas.formula
val fol_of_program_query :
  bool -> Expr2.expr -> string list * Fol.fol Formulas.formula
val disjoint_delta_sentence_of_stt :
  bool -> Expr2.expr -> Fol.fol Formulas.formula
val sourcestability_sentence_of_stt :
  bool -> Expr2.expr -> Fol.fol Formulas.formula
val getput_sentence_of_stt : bool -> Expr2.expr -> Fol.fol Formulas.formula
val putget_sentence_of_stt : bool -> Expr2.expr -> Fol.fol Formulas.formula
val constraint_sentence_of_stt :
  bool -> Expr2.expr -> Fol.fol Formulas.formula
val non_view_constraint_sentence_of_stt :
  bool -> Expr2.expr -> Fol.fol Formulas.formula
val view_constraint_sentence_of_stt :
  bool -> Expr2.expr -> Fol.fol Formulas.formula
val get_goal_predicate : string list -> int -> Expr2.rterm
val ranf2datalog :
  Fol.fol Formulas.formula ->
  string list ->
  int -> int -> (Expr2.rterm * Expr2.term list) list * Expr2.rterm * int
val datalog_of_conj :
  Fol.fol Formulas.formula list ->
  string list ->
  int -> int -> (Expr2.rterm * Expr2.term list) list * Expr2.rterm * int
val view_fol2datalog :
  bool ->
  Expr2.view ->
  Expr2.source list -> string list -> Fol.fol Formulas.formula -> Expr2.expr
val fol2datalog :
  bool ->
  bool ->
  Expr2.query ->
  Expr2.source list -> string list -> Fol.fol Formulas.formula -> Expr2.expr
val optimize_query_datalog : bool -> Expr2.expr -> Expr2.expr
