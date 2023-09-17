%token <int> INTEGER
%token <string> IDENT TEXT
%token <float> FLOAT
%token LPAREN RPAREN COMMA EOF DOT NULL
%token INSERT INTO VALUES DELETE FROM UPDATE WHERE EQUAL ASTERISK SET AND OR
%token NUM_DIV_OP NUM_NEQ_OP PLUS MINUS

%left OR
%left AND
%nonassoc EQUAL NUM_NEQ_OP
%left PLUS MINUS
%left ASTERISK NUM_DIV_OP
%nonassoc UNARY_MINUS

%start <Ast.statement> statement

%%

  statement:
  | insert EOF { $1 }
  | delete EOF { $1 }
  | update EOF { $1 }
  ;

  insert:
  | INSERT INTO table=IDENT VALUES vs=commas(values) { Ast.InsertInto (table, vs) }
  ;

  values:
  | LPAREN es=commas(vterm) RPAREN { es }
  ;

  delete:
  | DELETE FROM table=IDENT ws=wheres? { Ast.DeleteFrom (table, Option.value ~default:[] ws) }
  ;

  update:
  | UPDATE table=IDENT SET ss=commas(set_column) ws=wheres? { Ast.UpdateSet (table, ss, Option.value ~default:[] ws) }
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
  ;

  wheres:
  | WHERE ws=ors(where) { ws }
  ;

  where:
  | cs=ands(sql_constraint) { cs }
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
%inline ors(X): l=separated_nonempty_list(OR, X) { l }
%inline ands(X): l=separated_nonempty_list(AND, X) { l }
