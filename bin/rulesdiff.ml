open Birds

let parse_rule filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  Parser.main Lexer.token lexbuf

let _ =
  if Array.length Sys.argv < 3 then
    print_endline "Invalid arguments. A couple of file names must be passed."
  else begin
    let ast = parse_rule Sys.argv.(1) in
    let rules1 = Expr.RuleSet.of_list ast.rules in
    let rules2 = Expr.RuleSet.of_list (parse_rule Sys.argv.(2)).rules in
    let rules = Expr.RuleSet.diff rules1 rules2 |> Expr.RuleSet.to_seq |> List.of_seq in
    print_endline @@ Expr.to_string { ast with rules }
  end
