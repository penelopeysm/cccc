let indent_size = 2

let print_indent (indent_level : int) : unit =
  let indent_str = String.make (indent_level * indent_size) ' ' in
  Stdlib.prerr_string indent_str

type identifier = Identifier of string

let pp_identifier (Identifier id) (indent_level : int) : unit =
  print_indent indent_level;
  ANSITerminal.prerr_string [ Bold ] "Identifier";
  Stdlib.prerr_string " ";
  Stdlib.prerr_string id

type exp = IntLiteral of int

let pp_exp (e : exp) (indent_level : int) : unit =
  match e with
  | IntLiteral i ->
      print_indent indent_level;
      ANSITerminal.prerr_string [ Bold ] "IntLiteral";
      Stdlib.prerr_string " ";
      Stdlib.prerr_string (string_of_int i)

type statement = Return of exp

let pp_statement (s : statement) (indent_level : int) : unit =
  match s with
  | Return e ->
      print_indent indent_level;
      ANSITerminal.prerr_string [ Bold ] "Return\n";
      pp_exp e (indent_level + 1)

type func = Function of { name : identifier; body : statement }

let pp_func (f : func) (indent_level : int) : unit =
  match f with
  | Function { name; body } ->
      print_indent indent_level;
      ANSITerminal.prerr_string [ Bold ] "Function ";
      Stdlib.prerr_string (match name with Identifier id -> id);
      Stdlib.prerr_string "\n";
      pp_statement body (indent_level + 1)

type t = Programme of func

let pp_t (p : t) : unit =
  match p with
  | Programme f ->
      pp_func f 0;
      prerr_endline ""
