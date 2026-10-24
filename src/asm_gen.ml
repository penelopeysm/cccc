open! Core
open! Import

let asm_of_var = function
  | Ir.Var { identifier } -> Asm.PseudoRegister identifier
;;

let asm_of_value = function
  | Ir.Constant i -> Asm.Immediate i
  | Ir.Variable v -> asm_of_var v
;;

let asm_of_unary_op = function
  | Ir.Minus -> Asm.Neg
  | Ir.Complement -> Asm.Not
;;

let asm_of_instruction = function
  | Ir.Return { src } ->
    [ Asm.Movl { src = asm_of_value src; dst = Asm.Register Asm.AX }; Asm.Ret ]
  | Ir.UnaryOp { op; src; dst } ->
    [ Asm.Movl { src = asm_of_value src; dst = asm_of_var dst }
    ; Asm.Unary { op = asm_of_unary_op op; target = asm_of_var dst }
    ]
  | Ir.BinaryOp { op; left; right; dst } ->
    let addsubmul_helper asm_op =
      [ Asm.Movl { src = asm_of_value left; dst = asm_of_var dst }
      ; Asm.Binary { op = asm_op; left = asm_of_value right; dst = asm_of_var dst }
      ]
    in
    let divmod_helper result_register =
      [ (* move dividend to EAX, sign-extend, then call idivl *)
        Asm.Movl { src = asm_of_value left; dst = Asm.Register Asm.AX }
      ; Asm.Cdq
      ; Asm.Idivl { divisor = asm_of_value right }
      ; (* the quotient will now be in EAX and remainder in EDX *)
        Asm.Movl { src = result_register; dst = asm_of_var dst }
      ]
    in
    let shift_helper asm_op =
      let first_inst = Asm.Movl { src = asm_of_value left; dst = asm_of_var dst } in
      let remaining_insts =
        match right with
        | Ir.Constant immediate ->
          (* Easy peasy, just emit as-is *)
          [ Asm.Binary
              { op = asm_op; left = Asm.Immediate immediate; dst = asm_of_var dst }
          ]
        | Ir.Variable _ ->
          (* This is more complicated because SAL/SAR only work on the CL register (which
             is the lowest 8 bits of CX) *)
          [ Asm.Movl { src = asm_of_value right; dst = Asm.Register Asm.CX }
          ; Asm.Binary { op = asm_op; left = Asm.Register Asm.CX; dst = asm_of_var dst }
          ]
      in
      first_inst :: remaining_insts
    in
    (match op with
     | Ir.Add -> addsubmul_helper Asm.Add
     | Ir.Subtract -> addsubmul_helper Asm.Sub
     | Ir.Multiply -> addsubmul_helper Asm.Imul
     | Ir.Divide -> divmod_helper (Asm.Register Asm.AX)
     | Ir.Modulo -> divmod_helper (Asm.Register Asm.DX)
     | Ir.BitwiseAnd -> addsubmul_helper Asm.And
     | Ir.BitwiseOr -> addsubmul_helper Asm.Or
     | Ir.BitwiseXor -> addsubmul_helper Asm.Xor
     | Ir.ShiftLeft -> shift_helper Asm.Sal
     | Ir.ShiftRight -> shift_helper Asm.Sar)
;;

let asm_of_func = function
  | Ir.Func { name; insts } ->
    let insts = List.concat_map ~f:asm_of_instruction insts in
    let insts, stack_size = Fix_asm.replace_pseudoregisters insts in
    let insts =
      if Natural.is_zero stack_size
      then insts
      else Asm.AllocateStack { size = stack_size } :: insts
    in
    let insts = Fix_asm.fix_invalid_insts insts in
    Asm.Function { name; insts }
;;

let asm_of_ir = function
  | Ir.Programme func -> Asm.Programme { entry = asm_of_func func }
;;
