type error

val string_of_error : error -> string

(**
  * @param SQL statement.
  * @param columns List of column names of the table to be queried.
  *
  * @return List of rules in datalog language, or failure.
  *)
val to_datalog : Sql.Ast.statement -> Sql.Ast.column_name list -> (Expr.rule list, error) result
