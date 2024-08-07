open Birds

let parse_inlining_mode arg =
  if String.starts_with ~prefix:"+" arg then
    Inlining.InliningPredType.Deltainsert, String.sub arg 1 (String.length arg - 1)
  else if String.starts_with ~prefix:"-" arg then
    Inlining.InliningPredType.Deltadelete, String.sub arg 1 (String.length arg - 1)
  else
    Inlining.InliningPredType.Pred, arg

let _ =
  if Array.length Sys.argv < 2 then
    print_endline "Invalid arguments. File name must be passed."
  else begin
    let filename = Sys.argv.(1) in
    let chan = open_in filename in
    let lexbuf = Lexing.from_channel chan in
    let ast = Parser.main Lexer.token lexbuf in
    let rules = ast.rules in
    let mode =
      if Array.length Sys.argv < 3 then
        Inlining.All
      else
        let target =
          Sys.argv.(2)
          |> parse_inlining_mode
          |> Inlining.TableNameSet.singleton
        in
        Inlining.Just target
    in
    match Inlining.inline_rules mode rules with
    | Result.Error err ->
      print_endline @@ Inlining.string_of_error err
    | Result.Ok rules ->
      let ast = Expr.{ ast with rules } in
      print_endline @@ Expr.to_string ast
  end
