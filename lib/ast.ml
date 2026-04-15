type identifier = Identifier of { name : string }

let get_identifier_name (Identifier { name }) : string = name

type unary_operator = Decrement | Minus | Complement
type binary_operator = Plus | Subtract | Multiply | Divide | Modulo

type exp =
  | IntLiteral of { value : int }
  | UnaryOp of { op : unary_operator; operand : exp }
  | BinaryOp of {
      op : binary_operator;
      left_operand : exp;
      right_operand : exp;
    }

type statement = Return of { return_value : exp }
type func = Function of { name : identifier; body : statement }
type t = Programme of { entry : func }

module Pp : sig
  val pp : t -> Buffer.t
end = struct
  let indent_size = 2

  let add_indent (buf : Buffer.t) (indent_level : int) : unit =
    Buffer.add_string buf (String.make (indent_level * indent_size) ' ')

  let pp_identifier (buf : Buffer.t) (Identifier { name }) (indent_level : int)
      : unit =
    add_indent buf indent_level;
    Buffer.add_string buf "Identifier ";
    Buffer.add_string buf name

  let pp_unary_op (buf : Buffer.t) (op : unary_operator) (indent_level : int) :
      unit =
    add_indent buf indent_level;
    Buffer.add_string buf "UnaryOperator ";
    Buffer.add_string buf
      (match op with Decrement -> "--" | Minus -> "-" | Complement -> "~");
    Buffer.add_string buf "\n"

  let pp_binary_op (buf : Buffer.t) (op : binary_operator) (indent_level : int)
      : unit =
    add_indent buf indent_level;
    Buffer.add_string buf "BinaryOperator ";
    Buffer.add_string buf
      (match op with
      | Plus -> "+"
      | Subtract -> "-"
      | Multiply -> "*"
      | Divide -> "/"
      | Modulo -> "%");
    Buffer.add_string buf "\n"

  let rec pp_exp (buf : Buffer.t) (e : exp) (indent_level : int) : unit =
    match e with
    | IntLiteral { value } ->
        add_indent buf indent_level;
        Buffer.add_string buf "IntLiteral ";
        Buffer.add_string buf (string_of_int value)
    | UnaryOp { op; operand } ->
        add_indent buf indent_level;
        Buffer.add_string buf "UnaryOp\n";
        pp_unary_op buf op (indent_level + 1);
        pp_exp buf operand (indent_level + 1)
    | BinaryOp { op; left_operand; right_operand } ->
        add_indent buf indent_level;
        Buffer.add_string buf "BinaryOp\n";
        pp_binary_op buf op (indent_level + 1);
        pp_exp buf left_operand (indent_level + 1);
        Buffer.add_string buf "\n";
        pp_exp buf right_operand (indent_level + 1)

  let pp_statement (buf : Buffer.t) (s : statement) (indent_level : int) : unit
      =
    match s with
    | Return { return_value } ->
        add_indent buf indent_level;
        Buffer.add_string buf "Return\n";
        pp_exp buf return_value (indent_level + 1)

  let pp_func (buf : Buffer.t) (Function { name; body }) (indent_level : int) :
      unit =
    add_indent buf indent_level;
    Buffer.add_string buf "Function ";
    Buffer.add_string buf (get_identifier_name name);
    Buffer.add_string buf "\n";
    pp_statement buf body (indent_level + 1)

  let pp (Programme { entry }) : Buffer.t =
    let buf = Buffer.create 256 in
    pp_func buf entry 0;
    buf
end
