type operand = Register | Immediate of int
type instruction = Mov of { src : operand; dst : operand } | Ret
type func = { name : string; insts : instruction list }
type t = { entry : func }

let lower_exp (exp : Ast.exp) : operand =
  match exp with Ast.IntLiteral i -> Immediate i

let lower_statement (stmt : Ast.statement) : instruction list =
  match stmt with
  | Ast.Return exp -> [ Mov { src = lower_exp exp; dst = Register }; Ret ]

let lower_func (f : Ast.func) : func =
  match f with
  | Ast.Function { name = Ast.Identifier id; body } ->
      let insts = List.concat_map lower_statement [ body ] in
      { name = id; insts }

let lower (ast : Ast.t) : t =
  match ast with Ast.Programme func -> { entry = lower_func func }
