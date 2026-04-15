module Builder = struct
  type t = {
    (* build from the head of the list then reverse at the end *)
    mutable rev_insts : Ir.instruction list;
    mutable variable_counter : int;
  }

  let create () : t = { rev_insts = []; variable_counter = 0 }

  let cat_inst (inst : Ir.instruction) (b : t) : unit =
    b.rev_insts <- inst :: b.rev_insts;
    ()

  let get_insts (b : t) : Ir.instruction list = List.rev b.rev_insts

  let make_temp_var (b : t) : Ir.var =
    let nm = string_of_int b.variable_counter in
    b.variable_counter <- b.variable_counter + 1;
    Ir.Var { identifier = nm }
end

let rec lower_exp (exp : Ast.exp) (b : Builder.t) : Ir.value =
  match exp with
  | Ast.IntLiteral { value } -> Ir.Constant value
  | Ast.UnaryOp { op; operand } ->
      let operand_value = lower_exp operand b in
      let dst_var = Builder.make_temp_var b in
      let ir_op =
        match op with
        | Ast.Minus -> Ir.Minus
        | Ast.Complement -> Ir.Complement
        | Ast.Decrement -> failwith "not implemented"
      in
      let new_op =
        Ir.UnaryOp { op = ir_op; src = operand_value; dst = dst_var }
      in
      Builder.cat_inst new_op b;
      Ir.Variable dst_var
  | Ast.BinaryOp { op; left_operand; right_operand } -> failwith "TODO"

let lower_statement (stmt : Ast.statement) : Ir.instruction list =
  let b = Builder.create () in
  match stmt with
  | Ast.Return { return_value } ->
      let retval = lower_exp return_value b in
      let return_op = Ir.Return { src = retval } in
      Builder.cat_inst return_op b;
      Builder.get_insts b

let lower_func = function
  | Ast.Function { name; body } ->
      Ir.Func
        { name = Ast.get_identifier_name name; insts = lower_statement body }

let ir_of_ast = function
  | Ast.Programme { entry } -> Ir.Programme (lower_func entry)
