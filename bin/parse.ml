open Birds

let check_arguments_count argv =
  if Array.length argv < 2 then
    Result.Error "Invalid arguments. SQL file name must be passed."
  else
    Result.Ok ()

let open_sql_ast filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let ast = Sql.Parser.statement Sql.Lexer.token lexbuf in
  Result.Ok ast

let main =
  let open Utils.ResultMonad in
  check_arguments_count Sys.argv >>= fun _ ->
  open_sql_ast Sys.argv.(1) >>= fun sql ->
  return @@ print_endline @@ Sql.Ast.to_string sql

let _ =
  match main with
  | Result.Ok _ -> ()
  | Result.Error err -> print_endline err

