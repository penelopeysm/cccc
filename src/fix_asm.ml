open! Core
open! Import

module Fix_pseudo = struct
  type t =
    { (* NOTE: this maps pseudoregister names to POSITIVE values *)
      mutable pseudos_to_stackpos : Natural.t String.Map.t
    ; mutable required_stack_space : Natural.t
    }

  let create () : t =
    { pseudos_to_stackpos = String.Map.empty; required_stack_space = Natural.zero }
  ;;

  let fix_operand (a : t) (op : Asm.operand) =
    match op with
    | Asm.PseudoRegister nm ->
      (match Map.find a.pseudos_to_stackpos nm with
       | None ->
         let sz = Natural.create_exn 4 in
         (* increment before adding the memory location *)
         a.required_stack_space <- Natural.(a.required_stack_space + sz);
         let positive_stackval = a.required_stack_space in
         a.pseudos_to_stackpos
         <- Map.set a.pseudos_to_stackpos ~key:nm ~data:positive_stackval;
         Asm.Stack positive_stackval
       | Some positive_stackval -> Asm.Stack positive_stackval)
    | _ -> op
  ;;
end

let fix_inst (a : Fix_pseudo.t) (inst : Asm.instruction) : Asm.instruction =
  let open Fix_pseudo in
  match inst with
  (* no need to do anything *)
  | Asm.Ret | Asm.Cdq | Asm.AllocateStack _ -> inst
  (* patch operands *)
  | Asm.Movl { src; dst } -> Asm.Movl { src = fix_operand a src; dst = fix_operand a dst }
  | Asm.Unary { op; target } -> Asm.Unary { op; target = fix_operand a target }
  | Asm.Idivl { divisor } -> Asm.Idivl { divisor = fix_operand a divisor }
  | Asm.Binary { op; left; dst } ->
    Asm.Binary { op; left = fix_operand a left; dst = fix_operand a dst }
;;

(* Fixes up the instruction list to replace pseudoregisters with stack locations.
   Additionally returns the amount of stack space that is required to hold all the values
   that were previously stored in pseudoregisters. *)
let replace_pseudoregisters (insts : Asm.instruction list)
  : Asm.instruction list * Natural.t
  =
  let acc = Fix_pseudo.create () in
  (* NB: can't return this: (List.map (fun i -> fix_inst acc i) insts,
     acc.required_stack_space) because that evaluates the required stack space before the
     map and makes it always 0! *)
  let fixed_insts = List.map ~f:(fun i -> fix_inst acc i) insts in
  fixed_insts, acc.required_stack_space
;;

(** replace the following:
    - movl, add, sub, imul memory <=> memory
    - imul reg/mem/imm <=> memory
    - idiv with immediate value *)
let fix_invalid_insts (insts : Asm.instruction list) : Asm.instruction list =
  let rec f (inst : Asm.instruction) : Asm.instruction list =
    match inst with
    | Asm.Movl { src = Asm.Stack s1; dst = Asm.Stack s2 } ->
      [ Asm.Movl { src = Asm.Stack s1; dst = Asm.Register R10 }
      ; Asm.Movl { src = Asm.Register R10; dst = Asm.Stack s2 }
      ]
    | Asm.Binary { op; left = Asm.Stack s1; dst = Asm.Stack s2 } ->
      (* binary operations don't work with memory <=> memory, so we move the first of them
         into a scratch register *)
      Asm.Movl { src = Asm.Stack s1; dst = Asm.Register R10 }
      (* The following binary operation might still have to be fixed, for example if it's
         an imul which can't take a memory destination as the destination. Easy fix: just
         recursively call f. *)
      :: f (Asm.Binary { op; left = Asm.Register R10; dst = Asm.Stack s2 })
    | Asm.Binary { op; left; dst = Asm.Stack s } ->
      (* this branch targets imul with a memory destination *)
      (match op with
       | Asm.Imul ->
         [ Asm.Movl { src = Asm.Stack s; dst = Asm.Register R11 }
         ; Asm.Binary { op; left; dst = Asm.Register R11 }
         ; Asm.Movl { src = Asm.Register R11; dst = Asm.Stack s }
         ]
       | _ -> [ inst ])
    | Asm.Idivl { divisor = Asm.Immediate i } ->
      [ Asm.Movl { src = Asm.Immediate i; dst = Asm.Register R10 }
      ; Asm.Idivl { divisor = Asm.Register R10 }
      ]
    | _ -> [ inst ]
  in
  List.concat_map ~f insts
;;
