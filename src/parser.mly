%{ (* OCaml preamble *)

<<<<<<< HEAD
  open Expr
  open Utils
=======
  open Expr2 
  open Utils 
>>>>>>> d96beeb (introduce files)
  (* let parse_error (s : string) = spec_parse_error s 1 *)
  (* end preamble *)
 %}


/* tokens declaration */

%token <int> INT        /* token with int value    */
%token <float> FLOAT    /* token with float value  */
%token <string> STRING  /* token with string value */
<<<<<<< HEAD
%token <string> RELNAME /* token with string value */
=======
%token <string> RELNAME /* token with string value also used as column name, etc */
>>>>>>> d96beeb (introduce files)
%token <string> VARNAME /* token with string value */

%token QMARK SMARK VMARK DOT IMPLIEDBY PK
%token TYPING SINT SREAL SSTRING SBOOL
%token AND NOT OR TT FF BOT TOP
%token NULL
%token EQ
%token NE LE GE LT GT
%token PLUS MINUS TIMES DIVIDE CONCAT
%token LPAREN RPAREN LBRACKET RBRACKET SEP
%token EOP
%token EOF
%token ANONVAR /* anonymous variable */
%token ANON    /* fake token to stop the grammar in the fact rule */
<<<<<<< HEAD
=======
%token SELECT FROM WHERE INSERT DELETE UPDATE SET INTO VALUES EXISTS
>>>>>>> d96beeb (introduce files)

/* associativity and precedence when needed */
%nonassoc IMPLIEDBY


%start main               /* entry point */
<<<<<<< HEAD
%type <Expr.expr> main

%start parse_rterm
%type <Expr.rterm> parse_rterm

%start parse_query
%type <Expr.conj_query> parse_query
=======
%type <Expr2.expr> main

%start parse_rterm
%type <Expr2.rterm> parse_rterm

%start parse_query
%type <Expr2.conj_query> parse_query
>>>>>>> d96beeb (introduce files)

%%

/* Grammar */
  main:
  | EOF         { get_empty_expr }
  | program EOF { $1 }
  | error       { spec_parse_error "invalid syntax for a main program" 1; }
  ;

  parse_rterm:
  | predicate EOF { $1 }
  | error         { spec_parse_error "invalid syntax for a rterm" 1; }
  ;

  parse_query:
  | conj_query EOF { $1 }
  | error          { spec_parse_error "invalid syntax for a conj_query" 1; }
  ;
<<<<<<< HEAD

  program:
=======
  
  program: 
>>>>>>> d96beeb (introduce files)
  | exprlist { $1 }
  | error    { spec_parse_error "invalid syntax for a program" 1; }
  ;

  exprlist:
  | expr          { add_stt $1 get_empty_expr }
  | exprlist expr { add_stt $2 $1 }
  | error         { spec_parse_error "invalid syntax for a list of rules" 1; }
  ;

<<<<<<< HEAD
  expr:
=======
  expr: 
>>>>>>> d96beeb (introduce files)
  | primary_key          { Stt_Pk (fst $1, snd $1) }
  | integrity_constraint { Stt_Constraint (fst $1, snd $1) }
  | rule                 { Stt_Rule (fst $1, snd $1) }
  | source               { Stt_Source (fst $1, snd $1) }
  | view                 { Stt_View (fst $1, snd $1) }
  | fact                 { Stt_Fact $1 }
  | query                { Stt_Query $1 }
<<<<<<< HEAD
=======
  | update               { Stt_Update $1 }
>>>>>>> d96beeb (introduce files)
  | error                { spec_parse_error "invalid syntax for a rule or a declaration of query/source/view/constraint" 1; }
  ;

  primary_key:
  | PK LPAREN RELNAME SEP LBRACKET attrlist RBRACKET RPAREN	DOT	{ ($3, $6) }
  | PK LPAREN RELNAME SEP LBRACKET attrlist RBRACKET RPAREN EOF { spec_parse_error "miss a dot for a primary key" 3; }
  | error                                                       { spec_parse_error "invalid syntax for a primary key" 1; }
  ;

  integrity_constraint:
<<<<<<< HEAD
  | BOT IMPLIEDBY body DOT               { (get_empty_pred, $3) }
=======
  | BOT IMPLIEDBY body DOT               { (get_empty_pred, $3) } 
>>>>>>> d96beeb (introduce files)
  | BOT LPAREN RPAREN IMPLIEDBY body DOT { (Pred ("⊥", []), $5) }
  | BOT IMPLIEDBY body EOF               { spec_parse_error "miss a dot for a constraint" 3; }
  | error                                { spec_parse_error "invalid syntax for a constraint" 1; }
  ;

  rule:
  | head IMPLIEDBY body DOT { ($1, $3) }
  | head IMPLIEDBY body EOF { spec_parse_error "miss a dot for a rule" 4; }
  | error                   { spec_parse_error "invalid syntax for a rule" 1; }
  ;

  source:
<<<<<<< HEAD
  | SMARK schema DOT { $2 }
=======
  | SMARK schema DOT { $2 } 
>>>>>>> d96beeb (introduce files)
  | SMARK schema EOF { spec_parse_error "miss a dot for a source relation" 3; }
  | error            { spec_parse_error "invalid syntax for a source relation" 1; }
  ;

  view:
<<<<<<< HEAD
  | VMARK schema DOT { $2 }
  | VMARK schema EOF { spec_parse_error "miss a dot for a view relation" 3; }
  | error            { spec_parse_error "invalid syntax for a view relation" 1; }
  ;

=======
  | VMARK schema DOT { $2 } 
  | VMARK schema EOF { spec_parse_error "miss a dot for a view relation" 3; }
  | error            { spec_parse_error "invalid syntax for a view relation" 1; }
  ;
  
>>>>>>> d96beeb (introduce files)
  fact:
  | predicate DOT { $1 }
  | error         { spec_parse_error "invalid syntax for a fact" 1; }
  ;

  query:
<<<<<<< HEAD
  | QMARK predicate DOT { $2 }
=======
  | QMARK predicate DOT { $2 } 
>>>>>>> d96beeb (introduce files)
  | QMARK predicate EOF { spec_parse_error "miss a dot for a query" 3; }
  | error               { spec_parse_error "invalid syntax for a query" 1; }
  ;

  attrlist:
  | /* empty */         { [] }
  | STRING              { String.uppercase_ascii (String.sub $1 1 (String.length $1 - 2)) :: [] }
  | STRING SEP attrlist { String.uppercase_ascii (String.sub $1 1 (String.length $1 - 2)) :: $3 } /* \!/ rec. on the right */
  | error               { spec_parse_error "invalid syntax for a list of attributes" 1; }
  ;

  head:
  | predicate { $1 }
  | error     { spec_parse_error "invalid syntax for a head" 1; }
  ;

  conj_query:
<<<<<<< HEAD
  | LPAREN varlist RPAREN IMPLIEDBY signed_literals
    {
      let pos_literals, neg_literal = $5 in
      Expr.Conj_query ($2, pos_literals, neg_literal)
=======
  | LPAREN varlist RPAREN IMPLIEDBY signed_literals 
    {
      let pos_literals, neg_literal = $5 in
      Expr2.Conj_query ($2, pos_literals, neg_literal)
>>>>>>> d96beeb (introduce files)
    }
  | error { spec_parse_error "invalid syntax for a conjunctive query" 1; }
  ;

  signed_literals:
  | predicate SEP signed_literals     { let pos, neg = $3 in $1 :: pos, neg }
  | NOT predicate SEP signed_literals { let pos, neg = $4 in pos, $2 :: neg }
  | predicate                         { [$1], [] }
  | NOT predicate                     { [], [$2] }
  | error                             { spec_parse_error "invalid syntax for a signed_literals" 1; }
  ;

  body:
  | litlist { List.rev $1 }
  | error   { spec_parse_error "invalid syntax for a body" 1; }
  ;

  schema:
  | RELNAME LPAREN attrtypelist RPAREN { ($1, $3) }
  | error                              { spec_parse_error "invalid syntax for a predicate" 1; }
  ;

  attrtypelist:
  | /* empty */                          { [] }
  | STRING TYPING stype                  { (String.uppercase_ascii (String.sub $1 1 (String.length $1 - 2)), $3) :: [] }
  | STRING TYPING stype SEP attrtypelist { (String.uppercase_ascii (String.sub $1 1 (String.length $1 - 2)), $3) :: $5 } /* \!/ rec. on the right */
  | error                                { spec_parse_error "invalid syntax for a list of pairs of an attribute and its type" 1; }
  ;

  stype:
  | SINT    { Sint }
  | SREAL   { Sreal }
  | SSTRING { Sstring }
  | SBOOL   { Sbool }
  ;

  litlist:
  | /* empty */         { [] }
  | literal             { $1 :: [] }
  | litlist AND literal { $3 :: $1 }
  | litlist SEP literal { $3 :: $1 }
  | error               { spec_parse_error "invalid syntax for a conjunction of literals" 1; }
  ;

  literal:
  | predicate     { Rel $1 }
  | NOT predicate { Not $2 }
  | equation      { Equat $1 }
  | NOT equation  { Noneq $2 }
  | error         { spec_parse_error "invalid syntax for a literal" 1; }
  ;

  predicate:
  | RELNAME LPAREN varlist RPAREN       { Pred ($1, $3) }
  | PLUS RELNAME LPAREN varlist RPAREN  { Deltainsert ($2, $4) }
  | MINUS RELNAME LPAREN varlist RPAREN { Deltadelete ($2, $4) }
  | error                               { spec_parse_error "invalid syntax for a predicate" 1; }
  ;

<<<<<<< HEAD
  equation:
=======
  equation:	
>>>>>>> d96beeb (introduce files)
  | value_expression EQ value_expression { Equation ( "=", $1, $3) }
  | value_expression NE value_expression { Equation ("<>", $1, $3) }
  | value_expression LT value_expression { Equation ( "<", $1, $3) }
  | value_expression GT value_expression { Equation ( ">", $1, $3) }
  | value_expression LE value_expression { Equation ("<=", $1, $3) }
  | value_expression GE value_expression { Equation (">=", $1, $3) }
  | error                                { spec_parse_error "invalid syntax for a comparison" 1; }
  ;

  value_expression:
  | term                         { $1 }
  | value_expression PLUS term   { BinaryOp ("+", $1, $3) }
  | value_expression CONCAT term { BinaryOp ("^", $1, $3) }
  | value_expression MINUS term  { BinaryOp ("-", $1, $3) }
  | error                        { spec_parse_error "invalid syntax for a arithmetic expression" 1; }
  ;

  term:
  | factor             { $1 }
  | term TIMES factor  { BinaryOp ("*", $1, $3) }
  | term DIVIDE factor { BinaryOp ("/", $1, $3) }
  | error              { spec_parse_error "invalid syntax for a term" 1; }
  ;

  factor:
  | value_primary { $1 }
  | error         { spec_parse_error "invalid syntax for a factor" 1; }
  ;

  value_primary:
  | parenthesized_value_expression       { $1 }
  | MINUS parenthesized_value_expression { UnaryOp ("-", $2) }
  | nonparenthesized_value_primary       { $1 }
  | error                                { spec_parse_error "invalid syntax for a primary number" 1; }
  ;

  nonparenthesized_value_primary:
  | constant   { Const $1 }
  | var_or_agg { Var $1 }
  | error      { spec_parse_error "invalid syntax for a primar number" 1; }
  ;

  parenthesized_value_expression:
  | LPAREN value_expression RPAREN { $2 }
  | error                          { spec_parse_error "invalid syntax for a parenthesized expression" 1; }
  ;

  var_or_agg:
  | VARNAME   { NamedVar $1 }
  | aggregate { $1 }
  | error     { spec_parse_error "invalid syntax for a var or a aggreation" 1; }
  ;

<<<<<<< HEAD
=======
  constlist:
  | constant  {$1 :: []}
  | constant SEP constlist { $1 :: $3 }
  ;

>>>>>>> d96beeb (introduce files)
  constant:
  | INT         { Int $1 }
  | MINUS INT   { Int (- $2) }
  | FLOAT       { Real $1 }
  | MINUS FLOAT { Real (-. $2) }
  | STRING      { String $1 }
  | NULL        { Null }
  | FF          { Bool false }
  | TT          { Bool true }
  | error       { spec_parse_error "invalid syntax for a constant" 1; }
  ;

  varlist:
  | /* empty */     { [] }
  | var             { $1 :: [] }
  | var SEP varlist { $1 :: $3 } /* \!/ rec. on the right */
  | error           { spec_parse_error "invalid syntax for a list of variables" 1; }
  ;

  var:
  | VARNAME   { NamedVar $1 }
  | ANONVAR   { AnonVar }
  | constant  { ConstVar $1 }
  | aggregate { $1 }
  | error     { spec_parse_error "invalid syntax for a variables" 1; }
  ;

  aggregate:
  | VARNAME LPAREN VARNAME RPAREN { AggVar ($1,$3) }
  | error                         { spec_parse_error "invalid syntax for a aggregation" 1; }
  ;
<<<<<<< HEAD
=======

  usetlist:
  | uset        { $1 :: [] }
  | uset SEP usetlist {$1 :: $3 }
  | error     { spec_parse_error "invalid syntax for a update set" 1; }
  ;

  uset:
  | VARNAME EQ value_expression { ($1, $3) }

  condlist:
  | cond      { $1 :: [] }
  | cond AND condlist { $1 :: $3 }
  ;

  cond:	
  | VARNAME EQ value_expression { Condition ( "=", $1, $3) }
  | VARNAME NE value_expression { Condition ("<>", $1, $3) }
  | VARNAME LT value_expression { Condition ( "<", $1, $3) }
  | VARNAME GT value_expression { Condition ( ">", $1, $3) }
  | VARNAME LE value_expression { Condition ("<=", $1, $3) }
  | VARNAME GE value_expression { Condition (">=", $1, $3) }
  | EXISTS LPAREN sql RPAREN                 { Exists ($3) }
  | NOT EXISTS LPAREN sql RPAREN             { Notexists ($4) }
  | error                                { spec_parse_error "invalid syntax for a comparison" 1; }
  ;

  update:
  | UPDATE RELNAME SET usetlist WHERE condlist { Update ($2, $4, $6) }
  | INSERT INTO RELNAME VALUES LPAREN constlist RPAREN { Insert1 ($3, $6) }
  | INSERT INTO RELNAME sql { Insert2 ($3, $4)}
  | DELETE FROM RELNAME WHERE condlist { Delete ($3, $5) }
  ;

  sql:
  | SELECT collist FROM rellist WHERE condlist { Select ($2, $4, $6) }      

  collist:
  | RELNAME    { $1 :: [] }
  | RELNAME SEP collist  { $1 :: $3 }
  ;
      
  rellist:
  | RELNAME    { $1 :: [] }
  | RELNAME SEP collist  { $1 :: $3 }
  ;
>>>>>>> d96beeb (introduce files)
