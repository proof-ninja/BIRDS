open Birds

let _ =
  if Array.length Sys.argv < 2 then
    print_endline "Invalid arguments. File name must be passed."
  else begin
    let filename = Sys.argv.(1) in
    let chan = open_in filename in
    let lexbuf = Lexing.from_channel chan in
    let ast = Parser.main Lexer.token lexbuf in
    match Inlining.sort_rules ast.rules with
    | Result.Error err ->
      print_endline @@ Inlining.string_of_error err
    | Result.Ok rules ->
      match Simplification.simplify rules with
      | Result.Error err ->
        print_endline @@ Simplification.string_of_error err
      | Result.Ok rules ->
        let ast = { ast with rules = rules } in
        match Ast2sql.convert_expr_to_operation_based_sql ast with
        | Result.Error err ->
          print_endline @@ Ast2sql.show_error err
        | Result.Ok operations ->
          List.iter (fun op ->
            print_endline @@ Ast2sql.stringify_sql_operation op
          ) operations
  end
