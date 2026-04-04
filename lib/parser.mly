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
%token EOF

(* Keywords *)
%token INT
%token VOID
%token RETURN

%start <Ast.t> programme
%%

programme:
  | f = func; EOF { Programme(f) }

func:
  | INT; func_name = identifier; LEFT_PAREN; VOID; RIGHT_PAREN; LEFT_BRACE; stmt = statement; RIGHT_BRACE { Function{ name = func_name; body = stmt} }

statement:
  | RETURN; expr = expression; SEMICOLON { Return(expr) }

expression:
  | i = INTLIT { IntLiteral(i) }

identifier:
  | id = IDENTIFIER { Identifier(id) }
