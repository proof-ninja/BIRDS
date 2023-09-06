open Birds

let check_arguments_count argv =
  if Array.length argv < 3 then
    Result.Error "Invalid arguments. Both SQL file name and Datalog file name must be passed."
  else
    Result.Ok ()

let open_sql_ast filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let ast = Sql.Parser.statement Sql.Lexer.token lexbuf in
  Result.Ok ast

let open_view_ast filename =
  let filename = filename in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let ast = Parser.main Lexer.token lexbuf in
  Result.Ok ast

let extract_schema expr =
  match expr.Expr.view with
  | Some (_, cols) -> Result.Ok (List.map fst cols)
  | None -> Result.Error "Invalid schema file. A view definition must be."

let convert_to_dl sql cols =
  match Sql2ast.to_datalog sql cols with
  | Result.Ok _ as succ -> succ
  | Result.Error err -> Result.Error (Sql2ast.string_of_error err)

let main =
  let open Utils.ResultMonad in
  check_arguments_count Sys.argv >>= fun _ ->
  open_sql_ast Sys.argv.(1) >>= fun sql ->
  open_view_ast Sys.argv.(2) >>= fun expr ->
  extract_schema expr >>= fun cols ->
  convert_to_dl sql cols >>= fun rules ->
  return @@ print_endline @@ Expr.to_string Expr.{ expr with rules }

let _ =
  match main with
  | Result.Ok _ -> ()
  | Result.Error err -> print_endline err

