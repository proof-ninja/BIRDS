%token <int> INTEGER
%token <string> IDENT TEXT
%token <float> FLOAT
%token LPAREN RPAREN COMMA EOF DOT NULL
%token UPDATE WHERE EQUAL ASTERISK SET AND CONCAT_OP
%token NUM_DIV_OP NUM_NEQ_OP PLUS MINUS

%left CONCAT_OP
%left AND
%nonassoc EQUAL NUM_NEQ_OP
%left PLUS MINUS
%left ASTERISK NUM_DIV_OP
%nonassoc UNARY_MINUS

%start <Ast.update> update

%%

  update:
  | update_stmt EOF { $1 }
  ;

  update_stmt:
  | UPDATE table=IDENT SET ss=commas(set_column) w=where? { Ast.UpdateSet (table, ss, w) }
  ;

  set_column:
  | c=column EQUAL e=vterm { c, e }
  ;

  vterm:
  | const { Ast.Const $1 }
  | column { Ast.Column $1 }
  | unary_op { $1 }
  | left=vterm op=binary_op right=vterm { Ast.BinaryOp (op, left, right) }
  | LPAREN e=vterm RPAREN { e }
  ;


  const:
  | INTEGER { Ast.Int $1 }
  | FLOAT { Ast.Real $1 }
  | TEXT { Ast.String $1 }
  | NULL { Ast.Null }
  ;

  column:
  | table=IDENT DOT cname=IDENT { (Some table), cname }
  | cname=IDENT { None, cname }
  ;

  unary_op:
  | MINUS e=vterm %prec UNARY_MINUS { Ast.UnaryOp (Ast.Negate, e) }
  ;

  binary_op:
  | PLUS { Ast.Plus }
  | MINUS { Ast.Minus }
  | ASTERISK { Ast.Times }
  | NUM_DIV_OP { Ast.Divides }
  | CONCAT_OP { Ast.Lor }
  ;

  where:
  | WHERE cs=ands(sql_constraint) { Ast.Where cs }
  ;

  sql_constraint:
  | left=vterm op=operator right=vterm { Ast.Constraint (left, op, right) }
  ;

  operator:
  | EQUAL { Ast.RelEqual }
  | NUM_NEQ_OP { Ast.RelNotEqual }
  | op=IDENT { Ast.RelGeneral op }
  ;

%inline commas(X): l=separated_nonempty_list(COMMA, X) { l }
%inline ands(X): l=separated_nonempty_list(AND, X) { l }
