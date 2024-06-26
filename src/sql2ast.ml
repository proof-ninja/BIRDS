open Utils

module Sql = Sql.Ast

let ( >>= ) = ResultMonad.( >>= )

type error =
  | InvalidStatementType of { actual: string; expected: string }
  | InvalidColumnName of string

let string_of_error = function
  | InvalidColumnName name ->
      Printf.sprintf "Invalid Column Name: %s" name
  | InvalidStatementType { actual; expected } ->
      Printf.sprintf "Statement type '%s' was expected, but actually it was '%s'." actual expected

(** Column Name (as String) to Expr.var *)
module ColumnVarMap = Map.Make(String)

let rec ast_vterm_of_sql_vterm colvarmap = function
  | Sql.Const const ->
      ResultMonad.return @@ Expr.Const
      begin match const with
      | Sql.Int n -> Expr.Int n
      | Sql.Real f -> Expr.Real f
      | Sql.String s -> Expr.String s
      | Sql.Bool b -> Expr.Bool b
      | Sql.Null -> Expr.Null
      end
  | Sql.Column column ->
      let column_name = Sql.string_of_column_ignore_instance column in
      ColumnVarMap.find_opt column_name colvarmap
        |> Option.map (fun var -> Expr.Var var)
        |> Option.to_result ~none:(InvalidColumnName column_name)
  | Sql.UnaryOp (op, sql_vterm) ->
      ast_vterm_of_sql_vterm colvarmap sql_vterm >>= fun vterm ->
      let op = Sql.string_of_unary_operator op in
      ResultMonad.return (Expr.UnaryOp (op, vterm))
  | Sql.BinaryOp (op, left, right) ->
      ast_vterm_of_sql_vterm colvarmap left >>= fun left ->
      ast_vterm_of_sql_vterm colvarmap right >>= fun right ->
      let op = Sql.string_of_binary_operator op in
      ResultMonad.return (Expr.BinaryOp (op, left, right))

let ast_terms_of_sql_where_clause colvarmap sql_constraints =
  let ast_term_of_sql_constraint = function
    | Sql.Constraint (left, op, right) ->
        let op = Sql.string_of_operator op in
        ast_vterm_of_sql_vterm colvarmap left >>= fun left ->
        ast_vterm_of_sql_vterm colvarmap right >>= fun right ->
        ResultMonad.return (Expr.Equat (Expr.Equation (op, left, right))) in
  ResultMonad.mapM
    ast_term_of_sql_constraint
    sql_constraints

let build_effects colvarmap column_and_vterms =
  (*
   * For optimisation, generate terms in the delta-datalog language rules
   * that remove records where all columns to be updated in the SET clause
   * of the SQL are already after that update.
   *)
  column_and_vterms
    |> ResultMonad.mapM (fun (sql_col, sql_vterm) ->
      ast_vterm_of_sql_vterm colvarmap sql_vterm >>= fun vterm ->
      let column_name = Sql.string_of_column_ignore_instance sql_col in
      ColumnVarMap.find_opt column_name colvarmap
        |> Option.to_result ~none:(InvalidColumnName column_name)
        >>= fun var ->
      ResultMonad.return (Expr.Equat (Expr.Equation ("<>", Expr.Var var, vterm))))

let build_deletion_rules colvarmap where_clause table_name varlist effect_terms =
  (* Constraints corresponding to the WHERE clause. May be empty. *)
  where_clause
  |> ResultMonad.mapM (ast_terms_of_sql_where_clause colvarmap)
  >>= fun bodies ->

  (* Create a rule corresponding to the operation to delete the record to be updated. *)
  let delete_pred = Expr.Deltadelete (table_name, varlist) in
  let from = Expr.Pred (table_name, varlist) in

  bodies
  |> List.map (fun body -> delete_pred, (Expr.Rel from :: body @ effect_terms))
  |> ResultMonad.return

let build_creation_rule colvarmap colvarmap' column_and_vterms table_name columns varlist =
  (* Create an expression equivalent to a SET clause in SQL. *)
  column_and_vterms
    |> ResultMonad.mapM (fun (column, vterm) ->
      ast_vterm_of_sql_vterm colvarmap' vterm >>= fun vterm ->
      let column_name = Sql.string_of_column_ignore_instance column in
      ColumnVarMap.find_opt column_name colvarmap
        |> Option.map (fun var -> Expr.Equat (Expr.Equation ("=", Expr.Var var, vterm)))
        |> Option.to_result ~none:(InvalidColumnName column_name)
    ) >>= fun body ->

  (* Create a rule corresponding to the operation to insert the record to be updated. *)
  columns
    |> ResultMonad.mapM (fun column ->
      let column_name = Sql.string_of_column_ignore_instance (None, column) in
      ColumnVarMap.find_opt column_name colvarmap'
        |> Option.to_result ~none:(InvalidColumnName column_name)
    ) >>= fun delete_var_list ->
  let delete_pred = Expr.Deltadelete (table_name, delete_var_list) in
  let body = body @ [Expr.Rel delete_pred] in
  let insert_pred = Expr.Deltainsert (table_name, varlist) in
  ResultMonad.return (insert_pred, body)

module ColumnSet = Set.Make(String)

(** Create (column name as String, Expr.var) list. *)
let make_column_var_list make_var =
  List.mapi (fun idx column_name ->
    let var = make_var idx column_name in
    (None, column_name), var
  )

let make_colvarmap column_var_list =
  column_var_list
  |> List.map (fun (col, var) -> Sql.string_of_column_ignore_instance col, var)
  |> List.to_seq
  |> ColumnVarMap.of_seq

let insert_to_datalog table_name values (columns : Sql.column_name list) =
  let column_var_list =
    columns
    |> make_column_var_list (fun idx _ -> Expr.NamedVar (Printf.sprintf "GENV%d" (idx + 1)))
  in
  let colvarmap = make_colvarmap column_var_list in

  values
  |> ResultMonad.mapM (fun vs ->
    let vars = columns |> List.map (fun col -> Expr.NamedVar col) in
    let rterm = Expr.Deltainsert (table_name, vars) in
    List.map2 (fun colvar value ->
      let left = Expr.Var colvar in
      ast_vterm_of_sql_vterm colvarmap value >>= fun right ->
      ResultMonad.return (Expr.Equat (Expr.Equation ("=", left, right)))
    ) vars vs
    |> ResultMonad.sequence
    >>= fun terms ->
    ResultMonad.return (rterm, terms)
  )

let delete_to_datalog table_name where_clause (columns : Sql.column_name list) =
  let column_var_list =
    columns
    |> make_column_var_list (fun idx _ -> Expr.NamedVar (Printf.sprintf "GENV%d" (idx + 1)))
  in
  let colvarmap = make_colvarmap column_var_list in
  let varlist = column_var_list |> List.map snd in

  build_deletion_rules colvarmap where_clause table_name varlist []

let update_to_datalog table_name column_and_vterms where_clause (columns : Sql.column_name list) =

  (*
   * The column name and the name of the variable on the delta-datalog code corresponding to that column.
   * The variable names are generated by sequential numbering from `V0`.
   *)
  let column_var_list =
    columns
    |> make_column_var_list (fun idx _ -> Expr.NamedVar (Printf.sprintf "GENV%d" (idx + 1)))
  in
  let colvarmap = make_colvarmap column_var_list in

  (*
   * `varlist` is a list of variable names for each column in the table.
   * `in_set` is the set of column names that appear in the SET clause in that SQL update statement.
   *)
  let varlist, in_set =
    List.fold_right (fun (column, var) (varlist, in_set) ->
      match List.assoc_opt column column_and_vterms with
      | None ->
          (var :: varlist), in_set
      | Some _ ->
          let column_name = Sql.string_of_column_ignore_instance column in
          (var :: varlist), (ColumnSet.add column_name in_set)
      ) column_var_list ([], ColumnSet.empty)
  in

  (*
    * List of variable names corresponding to columns in the table,
    * but with new names for the columns to be updated (with the `_2` suffix).
    * The names used for columns not to be updated remain the same as those in `column_var_list`.
    *)
  let column_var_list' =
    columns
    |> make_column_var_list (fun idx column_name ->
      let column_name = Sql.string_of_column_ignore_instance (None, column_name) in
      if ColumnSet.exists (fun c -> c = column_name) in_set then
        Expr.NamedVar (Printf.sprintf "GENV%d_2" (idx + 1))
      else
        Expr.NamedVar (Printf.sprintf "GENV%d" (idx + 1))
    )
  in
  let colvarmap' = make_colvarmap column_var_list' in

  build_effects colvarmap column_and_vterms
  >>= fun effect_terms ->
  effect_terms
  |> ResultMonad.mapM (fun eff -> build_deletion_rules colvarmap where_clause table_name varlist [eff])
  |> ResultMonad.map List.flatten
  >>= fun deletes ->

  build_creation_rule colvarmap colvarmap' column_and_vterms table_name columns varlist
  >>= fun insert ->

  ResultMonad.return (deletes @ [insert])

let to_datalog (statement : Sql.statement) (columns : Sql.column_name list) : (Expr.rule list, error) result =
  match statement with
  | Sql.UpdateSet (table_name, column_and_vterms, where_clause) ->
      update_to_datalog table_name column_and_vterms where_clause columns
  | Sql.DeleteFrom (table_name, where_clause) ->
      delete_to_datalog table_name where_clause columns
  | Sql.InsertInto (table_name, values) ->
      insert_to_datalog table_name values columns
