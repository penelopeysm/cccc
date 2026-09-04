open! Core
open! Import

type var = Var of { identifier : string }

type value =
  | Constant of int
  | Variable of var

type unary_operator =
  | Minus
  | Complement

type binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight

type instruction =
  | Return of { src : value }
  | UnaryOp of
      { op : unary_operator
      ; src : value
      ; dst : var
      }
  | BinaryOp of
      { op : binary_operator
      ; left : value
      ; right : value
      ; dst : var
      }

type func =
  | Func of
      { name : string
      ; insts : instruction list
      }

type t = Programme of func

module Pp : sig
  val pp : t -> Buffer.t
end = struct
  let indent_size = 2

  let add_indent (buf : Buffer.t) (indent_level : int) : unit =
    Buffer.add_string buf (String.make (indent_level * indent_size) ' ')
  ;;

  let pp_var (buf : Buffer.t) = function
    | Var { identifier } -> Buffer.add_string buf @@ "Var(%" ^ identifier ^ ")"
  ;;

  let pp_value (buf : Buffer.t) = function
    | Constant i ->
      Buffer.add_string buf "Constant(";
      Buffer.add_string buf (string_of_int i);
      Buffer.add_string buf ")"
    | Variable v -> pp_var buf v
  ;;

  let pp_unary_operator (buf : Buffer.t) (unop : unary_operator) =
    Buffer.add_string
      buf
      (match unop with
       | Minus -> "Minus"
       | Complement -> "Complement")
  ;;

  let pp_binary_operator (buf : Buffer.t) (binop : binary_operator) =
    Buffer.add_string
      buf
      (match binop with
       | Add -> "Add"
       | Subtract -> "Subtract"
       | Multiply -> "Multiply"
       | Divide -> "Divide"
       | Modulo -> "Modulo"
       | BitwiseAnd -> "BitwiseAnd"
       | BitwiseOr -> "BitwiseOr"
       | BitwiseXor -> "BitwiseXor"
       | ShiftLeft -> "ShiftLeft"
       | ShiftRight -> "ShiftRight")
  ;;

  let pp_instruction (buf : Buffer.t) (indent_level : int) (inst : instruction) : unit =
    add_indent buf indent_level;
    match inst with
    | Return { src } ->
      Buffer.add_string buf "return ";
      pp_value buf src;
      Buffer.add_string buf "\n"
    | UnaryOp { op; src; dst } ->
      pp_var buf dst;
      Buffer.add_string buf " = Unary(op=";
      pp_unary_operator buf op;
      Buffer.add_string buf ", src=";
      pp_value buf src;
      Buffer.add_string buf ")\n"
    | BinaryOp { op; left; right; dst } ->
      pp_var buf dst;
      Buffer.add_string buf " = Binary(op=";
      pp_binary_operator buf op;
      Buffer.add_string buf ", left=";
      pp_value buf left;
      Buffer.add_string buf ", right=";
      pp_value buf right;
      Buffer.add_string buf ")\n"
  ;;

  let pp_func (buf : Buffer.t) (indent_level : int) = function
    | Func { name; insts } ->
      add_indent buf indent_level;
      Buffer.add_string buf ("func " ^ name ^ ":\n");
      List.iter ~f:(fun inst -> pp_instruction buf (indent_level + 1) inst) insts
  ;;

  let pp (ir : t) : Buffer.t =
    let buf = Buffer.create 256 in
    match ir with
    | Programme f ->
      Buffer.add_string buf "programme:\n";
      pp_func buf 1 f;
      buf
  ;;
end
