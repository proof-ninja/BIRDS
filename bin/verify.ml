open Birds

type args = {
  filename: string;
  timeout: int;
}

let default_timeout = 180
let default_args = {
  filename = "";
  timeout = default_timeout;
}

let rec parse_argv args = function
  | [] -> args
  | "--timeout" :: timeout :: tail ->
    begin match int_of_string_opt timeout with
    | Some timeout -> parse_argv { args with timeout } tail
    | None -> failwith @@ Printf.sprintf "Invalid timeout: %s" timeout
    end
  | filename :: tail -> parse_argv { args with filename } tail

let _ =
  let args = parse_argv default_args @@ List.tl @@ Array.to_list Sys.argv in
  let chan = open_in args.filename in
  let lexbuf = Lexing.from_channel chan in
  let ast = Parser.main Lexer.token lexbuf in
  Verify.verify true args.timeout ast
