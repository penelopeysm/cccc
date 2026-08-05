%{
open Ast
%}

(* Note that tokens need to be defined first before their precedence /
   associativity can be set. *)
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
%token AMPERSAND
%token PIPE
%token CARET
%token SHIFT_LEFT
%token SHIFT_RIGHT
%token BANG
%token LOGICAL_AND
%token LOGICAL_OR
%token EQUAL
%token NOT_EQUAL
%token LESS
%token GREATER
%token LESS_EQUAL
%token GREATER_EQUAL

(* Binary operator precedence: from lowest to highest

   Refer to: https://en.cppreference.com/c/language/operator_precedence -- note
   that lowest precedence is at the bottom of this list so the order is reversed
   here!

   Operator associativity is explicitly defined via '%left'. *)
%left LOGICAL_OR (* 12 *)
%left LOGICAL_AND (* 11 *)
%left PIPE (* 10 *)
%left CARET (* 9 *)
%left AMPERSAND (* 8 *)
%left EQUAL NOT_EQUAL (* 7 *)
%left LESS GREATER LESS_EQUAL GREATER_EQUAL (* 6 *)
%left SHIFT_LEFT SHIFT_RIGHT (* 5 *)
%left PLUS MINUS (* 4 *)
%left STAR SLASH PERCENT (* 3 *)

(* This doesn't have a production rule: the lexer never generates it.
   It is basically a phantom thing that is used only to make sure that unary
   operators bind tighter than binary ops. *)
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

  (* Prefix unary ops *)
  | MINUS; operand = expression %prec PREFIX { UnaryOp{ op = Minus; operand } }
  | TILDE; operand = expression %prec PREFIX { UnaryOp{ op = Complement; operand } }
  | BANG; operand = expression %prec PREFIX { UnaryOp{ op = LogicalNot; operand } }

  (* Binary ops *)
  | left_operand = expression; PLUS; right_operand = expression { BinaryOp { op = Add; left_operand; right_operand } }
  | left_operand = expression; MINUS; right_operand = expression { BinaryOp {op = Subtract; left_operand; right_operand } }
  | left_operand = expression; STAR; right_operand = expression { BinaryOp { op = Multiply; left_operand; right_operand } }
  | left_operand = expression; SLASH; right_operand = expression { BinaryOp { op = Divide; left_operand; right_operand } }
  | left_operand = expression; PERCENT; right_operand = expression { BinaryOp {op =  Modulo; left_operand; right_operand } }
  | left_operand = expression; AMPERSAND; right_operand = expression { BinaryOp { op = BitwiseAnd; left_operand; right_operand } }
  | left_operand = expression; PIPE; right_operand = expression { BinaryOp { op = BitwiseOr; left_operand; right_operand } }
  | left_operand = expression; CARET; right_operand = expression { BinaryOp { op = BitwiseXor; left_operand; right_operand } }
  | left_operand = expression; SHIFT_LEFT; right_operand = expression { BinaryOp { op = ShiftLeft; left_operand; right_operand } }
  | left_operand = expression; SHIFT_RIGHT; right_operand = expression { BinaryOp { op = ShiftRight; left_operand; right_operand } }
  | left_operand = expression; LOGICAL_AND; right_operand = expression { BinaryOp { op = LogicalAnd; left_operand; right_operand } }
  | left_operand = expression; LOGICAL_OR; right_operand = expression { BinaryOp { op = LogicalOr; left_operand; right_operand } }
  | left_operand = expression; EQUAL; right_operand = expression { BinaryOp { op = Equal; left_operand; right_operand } }
  | left_operand = expression; NOT_EQUAL; right_operand = expression { BinaryOp { op = NotEqual; left_operand; right_operand } }
  | left_operand = expression; LESS; right_operand = expression { BinaryOp { op = Less; left_operand; right_operand } }
  | left_operand = expression; GREATER; right_operand = expression { BinaryOp { op = Greater; left_operand; right_operand } }
  | left_operand = expression; LESS_EQUAL; right_operand = expression { BinaryOp { op = LessEqual; left_operand; right_operand } }
  | left_operand = expression; GREATER_EQUAL; right_operand = expression { BinaryOp { op = GreaterEqual; left_operand; right_operand } }

identifier:
  | id = IDENTIFIER { Identifier { name = id} }
