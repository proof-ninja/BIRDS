
type binary_operator =
  | Plus    (* + *)
  | Minus   (* - *)
  | Times   (* * *)
  | Divides (* / *)
  | Lor     (* || *)

type unary_operator =
  | Negate (* - *)

type operator =
  | RelEqual
  | RelNotEqual
  | RelGeneral of string

type table_name = string

type column_name = string

type instance_name = string

type column = instance_name option * column_name

type const =
  | Int of int
  | Real of float
  | String of string
  | Bool of bool 
  | Null

type vterm =
  | Const    of const
  | Column   of column
  | UnaryOp  of unary_operator * vterm
  | BinaryOp of binary_operator * vterm * vterm

type sql_constraint =
  | Constraint of vterm * operator * vterm

type where_clause =
  | Where of sql_constraint list

type update =
  | UpdateSet of table_name * (column * vterm) list * where_clause option

let string_of_binary_operator = function
  | Plus    -> "+"
  | Minus   -> "-"
  | Times   -> "*"
  | Divides -> "/"
  | Lor     -> "||"

let string_of_unary_operator = function
  | Negate -> "-"

let string_of_operator = function
  | RelEqual      -> "="
  | RelNotEqual   -> "<>"
  | RelGeneral op -> op

let string_of_column (instance_name, column) =
  match instance_name with
  | Some instance_name -> Printf.sprintf "%s.%s" instance_name column
  | None -> column

let string_of_column_ignore_instance (_, column) = column

let string_of_const = function
  | Int i -> string_of_int i
  | Real f -> string_of_float f
  | String s -> s
  | Bool b -> string_of_bool b
  | Null -> "NULL"

let rec string_of_vterm = function
  | Const c -> string_of_const c
  | Column c -> string_of_column c
  | UnaryOp (op, e) -> string_of_unary_operator op ^ string_of_vterm e
  | BinaryOp (op, left, right) ->
      Printf.sprintf "%s %s %s"
        (string_of_vterm left)
        (string_of_binary_operator op)
        (string_of_vterm right)

let string_of_constraint = function
  | Constraint (left, op, right) ->
      Printf.sprintf "%s %s %s"
        (string_of_vterm left)
        (string_of_operator op)
        (string_of_vterm right)

let to_string = function
  | UpdateSet (table_name, sets, where) ->
    let string_of_set (col, vterm) =
      Printf.sprintf "  %s = %s" (string_of_column col) (string_of_vterm vterm)
    in
    "UPDATE\n" ^
    "  " ^ table_name ^ "\n" ^
    "SET\n" ^ (
      sets
      |> List.map string_of_set
      |> String.concat "\n"
    ) ^
    match where with
    | None -> ""
    | Some (Where cs) ->
      "\nWHERE\n" ^ (
        cs
        |> List.map (fun c -> "  " ^ string_of_constraint c)
        |> String.concat "\n"
      )
    ^ "\n;"
