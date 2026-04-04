let emit_operand (buf : Buffer.t) (op : Asm.operand) : unit =
  match op with
  | Asm.Register -> Buffer.add_string buf "%eax"
  | Asm.Immediate i ->
      Buffer.add_char buf '$';
      Buffer.add_string buf (string_of_int i)

let emit_instruction (buf : Buffer.t) (inst : Asm.instruction) : unit =
  match inst with
  | Asm.Mov { src; dst } ->
      Buffer.add_string buf "  movl ";
      emit_operand buf src;
      Buffer.add_string buf ", ";
      emit_operand buf dst;
      Buffer.add_char buf '\n'
  | Asm.Ret -> Buffer.add_string buf "  ret\n"

let emit_func (buf : Buffer.t) (func : Asm.func) : unit =
  (* macOS symbol names are prefixed with underscore *)
  let mangled_name = "_" ^ func.name in
  let global_decl = "  .globl " ^ mangled_name in
  Buffer.add_string buf global_decl;
  Buffer.add_char buf '\n';
  let func_decl = mangled_name ^ ":" in
  Buffer.add_string buf func_decl;
  Buffer.add_char buf '\n';
  List.iter (emit_instruction buf) func.insts

let emit (asm : Asm.t) : string =
  let buf = Buffer.create 1024 in
  emit_func buf asm.entry;
  Buffer.contents buf
