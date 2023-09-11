{
    open Parser;;
    open Lexing;;

    let spec_error msg start finish =
      Printf.sprintf
        "File \"%s\", line %d, characters %d-%d: '%s'"
        start.pos_fname
        start.pos_lnum
        (start.pos_cnum - start.pos_bol)
        (finish.pos_cnum - finish.pos_bol)
        msg
    
    exception LexErr of string
    let spec_lex_error lexbuf =
      raise (LexErr (spec_error (lexeme lexbuf) (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)))

    let keywords = [
        "insert", INSERT;
        "INSERT", INSERT;
        "into", INTO;
        "INTO", INTO;
        "values", VALUES;
        "VALUES", VALUES;
        "update", UPDATE;
        "UPDATE", UPDATE;
        "where", WHERE;
        "WHERE", WHERE;
        "set", SET;
        "SET", SET;
        "and", AND;
        "AND", AND;
        "or", OR;
        "OR", OR;
    ]
}
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha) (alpha | digit | '_' )*
let wsp = [' ' '\r' '\t']

rule token = parse
  | wsp { token lexbuf }
  | '\n' | ';' { Lexing.new_line lexbuf; token lexbuf }
  | "--" (wsp | alpha | digit) ('\n' | eof) { Lexing.new_line lexbuf; token lexbuf }
  | digit+ as lxm { INTEGER (int_of_string lxm) }
  | digit* '.'? digit+ (['e' 'E'] ['-' '+']? digit+)? as lxm { FLOAT (float_of_string (lxm)) }
  | '\'' (('\'' '\'') | [^'\n''\''])* '\'' as lxm { TEXT lxm }
  | ident as lxm {
    match List.assoc_opt lxm keywords with
    | Some t -> t
    | None -> IDENT lxm
  }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '.' { DOT }
  | "NULL" | "null" { NULL }
  | '=' { EQUAL }
  | '*' { ASTERISK }
  | '/' { NUM_DIV_OP }
  | "!=" | "<>" { NUM_NEQ_OP }
  | '+' { PLUS }
  | '-' { MINUS }
  | eof { EOF }
  | _ { spec_lex_error lexbuf }
