(* Platform.system is defined via a Dune build rule *)
type platform_type = MacOS | Linux | Other

let platform =
  match Platform.system with "macosx" -> MacOS | "linux" -> Linux | _ -> Other

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
  (* On macOS, x86 symbol names are prefixed with underscores -- not necessary
     on Linux *)
  let mangled_name = if platform = MacOS then "_" ^ func.name else func.name in
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
  (* Disable executable stack on Linux *)
  if platform = Linux then
    Buffer.add_string buf "  .section .note.GNU-stack,\"\",@progbits\n";
  Buffer.contents buf
