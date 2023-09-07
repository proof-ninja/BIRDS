type error

val string_of_error : error -> string

(**
  * @param update AST of the SQL UPDATE statement.
  * @param columns List of column names of the table to be queried.
  *
  * @return List of rules in datalog language, or failure.
  *)
val update_to_datalog : Sql.Ast.update -> Sql.Ast.column_name list -> (Expr.rule list, error) result
