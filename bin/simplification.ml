open Birds

let build_view ast =
  if Array.length Sys.argv >= 3 && Sys.argv.(2) = "--prepare" then
    ast.Expr.view
  else
    None

let _ =
  if Array.length Sys.argv < 2 then
    print_endline "Invalid arguments. File name must be passed."
  else begin
    let filename = Sys.argv.(1) in
    let chan = open_in filename in
    let lexbuf = Lexing.from_channel chan in
    let ast = Parser.main Lexer.token lexbuf in
    let rules = ast.rules in
    let sources = ast.sources in
    let view = build_view ast in
    match Inlining.sort_rules rules with
    | Result.Error err ->
      print_endline @@ Inlining.string_of_error err
    | Result.Ok rules ->
      match Simplification.simplify rules view sources with
      | Result.Error err ->
        print_endline @@ Simplification.string_of_error err
      | Result.Ok rules ->
        let ast = Expr.{ ast with rules } in
        print_endline @@ Expr.to_string ast
  end
