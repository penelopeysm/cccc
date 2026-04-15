{
  open Lexing
  open Parser
  exception Lexing_error of string
}

let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = "\r\n" | '\r' | '\n'
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  | int { INTLIT (int_of_string (Lexing.lexeme lexbuf)) }
  | identifier { match Lexing.lexeme lexbuf with
      | "int" -> INT
      | "void" -> VOID
      | "return" -> RETURN
      | id -> IDENTIFIER id
    }
  | "--" { DECREMENT }
  | '-' { MINUS }
  | '~' { TILDE }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '{' { LEFT_BRACE }
  | '}' { RIGHT_BRACE }
  | ';' { SEMICOLON }
  | '+' { PLUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '%' { PERCENT }
  | eof { EOF }
  | _ { raise (Lexing_error ("Unexpected character: " ^ (Lexing.lexeme lexbuf)))
    }
