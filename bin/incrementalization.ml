open Birds

let _ =
  if Array.length Sys.argv < 2 then
    print_endline "Invalid arguments. File name must be passed."
  else begin
    let filename = Sys.argv.(1) in
    let chan = open_in filename in
    let lexbuf = Lexing.from_channel chan in
    let ast = Parser.main Lexer.token lexbuf in
    match Incrementalization.incrementalization_ast ast with
    | Result.Error err ->
      print_endline @@ Incrementalization.string_of_error err
    | Result.Ok rules ->
      let ast = Expr.{ ast with rules } in
      print_endline @@ Expr.to_string ast
  end
