open Birds
open Utils

let sort rules =
  let open Result in
  match Inlining.sort_rules rules with
  | Error err ->
      error @@ Inlining.string_of_error err
  | Ok rules ->
      ok rules

let simplify rules =
  let open Result in
  match Simplification.simplify rules with
  | Error err ->
      error @@ Simplification.string_of_error err
  | Result.Ok rules ->
      ok rules

let convert ast =
  let open Result in
  match Ast2sql.convert_expr_to_operation_based_sql ast with
  | Error err ->
      error @@ Ast2sql.show_error err
  | Result.Ok operations ->
      let result =
        operations
        |> List.map Ast2sql.stringify_sql_operation
        |> String.concat "\n"
      in
      ok result

let main (ast : Expr.expr) =
  let open ResultMonad in
  sort ast.rules >>= fun rules ->
  simplify rules >>= fun rules ->
  let ast = { ast with rules = rules } in
  convert ast

let _ =
  if Array.length Sys.argv < 2 then
    print_endline "Invalid arguments. File name must be passed."
  else begin
    let filename = Sys.argv.(1) in
    let chan = open_in filename in
    let lexbuf = Lexing.from_channel chan in
    let ast = Parser.main Lexer.token lexbuf in
    print_endline @@ Result.fold ~ok:Fun.id ~error:Fun.id @@ main ast
  end
