open Birds

let usage_message = "Usage: verify [--timeout <timeout>] <filename>"
let filename = ref ""
let timeout = ref 180
let parse_rest filename' = filename := filename'
let speclist = [
  ("--timeout", Arg.Set_int timeout, "Set the timeout in seconds");
]

let _ =
  Arg.parse speclist parse_rest usage_message;
  let chan = open_in !filename in
  let lexbuf = Lexing.from_channel chan in
  let ast = Parser.main Lexer.token lexbuf in
  Verify.verify true !timeout ast
