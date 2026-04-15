%{
open Ast
%}

%token <int> INTLIT
%token <string> IDENTIFIER
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token SEMICOLON
%token DECREMENT
%token TILDE
%token EOF
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token PERCENT

(* Operator precedence: from lowest to highest *) 
(* Operator associativity is explicitly defined in the %left *)
%left PLUS MINUS
%left STAR SLASH PERCENT
(* This doesn't have a production rule: the lexer never generates it.
   It is basically a phantom thing that is used only to make sure that unary
   minus binds tighter than binary ops. *)
%nonassoc PREFIX

(* Keywords *)
%token INT
%token VOID
%token RETURN

%start <Ast.t> programme
%%

programme:
  | f = func; EOF { Programme { entry = f } }

func:
  | INT; func_name = identifier; LEFT_PAREN; VOID; RIGHT_PAREN; LEFT_BRACE; stmt = statement; RIGHT_BRACE { Function{ name = func_name; body = stmt} }

statement:
  | RETURN; expr = expression; SEMICOLON { Return {return_value = expr} }

expression:
  | i = INTLIT { IntLiteral { value = i}}
  | LEFT_PAREN; expr = expression; RIGHT_PAREN { expr }

  | MINUS; operand = expression %prec PREFIX { UnaryOp{ op = Minus; operand } }
  | TILDE; operand = expression { UnaryOp{ op = Complement; operand } }

  | left_operand = expression; PLUS; right_operand = expression { BinaryOp { op = Plus; left_operand; right_operand } }
  | left_operand = expression; MINUS; right_operand = expression { BinaryOp {op = Subtract; left_operand; right_operand } }
  | left_operand = expression; STAR; right_operand = expression { BinaryOp { op = Multiply; left_operand; right_operand } }
  | left_operand = expression; SLASH; right_operand = expression { BinaryOp { op = Divide; left_operand; right_operand } }
  | left_operand = expression; PERCENT; right_operand = expression { BinaryOp {op =  Modulo; left_operand; right_operand } }

identifier:
  | id = IDENTIFIER { Identifier { name = id} }
