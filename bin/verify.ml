open Birds

let _ =
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let ast = Parser.main Lexer.token lexbuf in
  Verify.verify true 600 ast
