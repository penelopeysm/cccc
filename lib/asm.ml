type register = AX | BX | CX | DX | R10 | R11

(* 8 bytes, 4 bytes, 2 bytes, 1 byte *)
type size = QWord | DWord | Word | Byte

type operand =
  | Register of register
  | Immediate of int
  | PseudoRegister of string
  (* convention here is that Stack holds negative numbers *)
  | Stack of int

type unary_operator = Neg | Not

(* For right shift, we need SAR for signed operands and SHR for unsigned. Right
   now we only have signed operands (i.e. int literals and combinations thereof)
   so we only have SAR. For left shift SAL and SHL are the same.

   Note that `And` and `Or` are the bitwise operations, not logical.
   *)
type binary_operator = Add | Sub | Imul | And | Or | Xor | Sal | Sar

type instruction =
  | Movl of { src : operand; dst : operand }
  | Ret
  | Unary of { op : unary_operator; target : operand }
  | Binary of { op : binary_operator; left : operand; dst : operand }
  | Cdq
  | Idivl of { divisor : operand }
  (* the size here is meant to be non-negative *)
  | AllocateStack of { size : int }

type func = Function of { name : string; insts : instruction list }
type t = Programme of { entry : func }

module Emit : sig
  (* convenience export for the validator below *)
  val string_of_instruction : instruction -> string
  val string_of_asm : t -> string
end = struct
  (* Platform.system is defined via a Dune build rule *)
  type platform_type = MacOS | Linux | Other

  let platform =
    match Platform.system with
    | "macosx" -> MacOS
    | "linux" -> Linux
    | _ -> Other

  let emit_register ?(sz : size = DWord) (buf : Buffer.t) (reg : register) :
      unit =
    let reg_name =
      match (sz, reg) with
      | QWord, AX -> "%rax"
      | DWord, AX -> "%eax"
      | Word, AX -> "%ax"
      | Byte, AX -> "%al"
      | QWord, BX -> "%rbx"
      | DWord, BX -> "%ebx"
      | Word, BX -> "%bx"
      | Byte, BX -> "%bl"
      | QWord, CX -> "%rcx"
      | DWord, CX -> "%ecx"
      | Word, CX -> "%cx"
      | Byte, CX -> "%cl"
      | QWord, DX -> "%rdx"
      | DWord, DX -> "%edx"
      | Word, DX -> "%dx"
      | Byte, DX -> "%dl"
      | QWord, R10 -> "%r10"
      | DWord, R10 -> "%r10d"
      | Word, R10 -> "%r10w"
      | Byte, R10 -> "%r10b"
      | QWord, R11 -> "%r11"
      | DWord, R11 -> "%r11d"
      | Word, R11 -> "%r11w"
      | Byte, R11 -> "%r11b"
    in
    Buffer.add_string buf reg_name

  let emit_operand ?(register_size : size = DWord) (buf : Buffer.t)
      (op : operand) : unit =
    match op with
    | Register r -> emit_register ~sz:register_size buf r
    | Immediate i ->
        Buffer.add_char buf '$';
        Buffer.add_string buf (string_of_int i)
    | PseudoRegister s ->
        Buffer.add_string buf "Pseudoregister(";
        Buffer.add_string buf s;
        Buffer.add_string buf ")"
    | Stack stackpos ->
        Buffer.add_string buf (string_of_int stackpos);
        Buffer.add_string buf "(%rbp)"

  let emit_instruction (buf : Buffer.t) (inst : instruction) : unit =
    match inst with
    | Movl { src; dst } ->
        Buffer.add_string buf "   movl ";
        emit_operand buf src;
        Buffer.add_string buf ", ";
        emit_operand buf dst;
        Buffer.add_char buf '\n'
    | Ret ->
        (* restore caller's stack pointer *)
        Buffer.add_string buf "   movq %rbp, %rsp\n";
        Buffer.add_string buf "   popq %rbp\n";
        (* the retval will already have been moved to AX *)
        Buffer.add_string buf "    ret\n"
    | Unary { op; target } ->
        Buffer.add_string buf
          (match op with Neg -> "   negl " | Not -> "   notl ");
        emit_operand buf target;
        Buffer.add_char buf '\n'
    | Binary { op; left; dst } ->
        Buffer.add_string buf
          (match op with
          | Add -> "   addl "
          | Sub -> "   subl "
          | Imul -> "  imull "
          | And -> "   andl "
          | Or -> "   orl "
          | Xor -> "   xorl "
          | Sal -> "   sall "
          | Sar -> "   sarl ");
        let left_sz = match op with Sal | Sar -> Byte | _ -> DWord in
        emit_operand ~register_size:left_sz buf left;
        Buffer.add_string buf ", ";
        emit_operand buf dst;
        Buffer.add_char buf '\n'
    | Cdq -> Buffer.add_string buf "   cdq\n"
    | Idivl { divisor } ->
        Buffer.add_string buf "  idivl ";
        emit_operand buf divisor;
        Buffer.add_char buf '\n'
    | AllocateStack { size } ->
        Buffer.add_string buf "   subq $";
        Buffer.add_string buf (string_of_int size);
        Buffer.add_string buf ", %rsp\n"

  let string_of_instruction (inst : instruction) : string =
    let buf = Buffer.create 64 in
    emit_instruction buf inst;
    Buffer.contents buf

  let emit_func (buf : Buffer.t) (func : func) : unit =
    match func with
    | Function { name; insts } ->
        (* On macOS, x86 symbol names are prefixed with underscores -- not necessary
       on Linux *)
        let mangled_name = if platform = MacOS then "_" ^ name else name in
        let global_decl = "  .globl " ^ mangled_name in
        Buffer.add_string buf global_decl;
        Buffer.add_char buf '\n';
        let func_decl = mangled_name ^ ":" in
        Buffer.add_string buf func_decl;
        Buffer.add_char buf '\n';
        (* enter new stack frame *)
        Buffer.add_string buf "  pushq %rbp\n";
        Buffer.add_string buf "   movq %rsp, %rbp\n";
        List.iter (emit_instruction buf) insts

  let string_of_asm = function
    | Programme { entry } ->
        let buf = Buffer.create 1024 in
        emit_func buf entry;
        (* Disable executable stack on Linux *)
        if platform = Linux then
          Buffer.add_string buf "   .section .note.GNU-stack,\"\",@progbits\n";
        Buffer.contents buf
end

(* The ASM type above is more lax than it should be because it can represent
   instructions that are invalid. The most obvious case is a pseudoregister
   that has not actually been allocated to a real register or stack slot.
   However there are also some other constraints, for example the fact
   that the destination of an `addl` instruction must not be an immediate
   value.
   It is the job of the fixup phase to ensure that these constraints are
   satisfied. This module helps that by providing a tool to check that this is
   indeed the case to avoid emitting malformed assembly code.
   *)
module Validate : sig
  val validate : t -> bool
end = struct
  (* This basically means not a pseudoregister. *)
  let is_reg_mem_or_imm = function PseudoRegister _ -> false | _ -> true
  let is_reg_or_mem = function Register _ | Stack _ -> true | _ -> false
  let is_reg = function Register _ -> true | _ -> false
  let is_imm = function Immediate _ -> true | _ -> false

  let validate_binary op left dst =
    (* destination can't be an immediate *)
    is_reg_mem_or_imm left && is_reg_or_mem dst
    (* can't do mem to mem *)
    && (match (left, dst) with Stack _, Stack _ -> false | _ -> true)
    (* and now for operation-specific constraints *)
    &&
    match op with
    | Add | Sub | And | Or | Xor -> true
    (* SAL/SAR shift amount must either be in CL or an immediate *)
    | Sal | Sar -> left = Register CX || is_imm left
    (* imul destination must be a register *)
    | Imul -> is_reg dst

  let validate_instruction = function
    | Movl { src; dst } -> (
        is_reg_mem_or_imm src && is_reg_or_mem dst
        (* No memory to memory moves. *)
        && match (src, dst) with Stack _, Stack _ -> false | _ -> true)
    | Ret | Cdq -> true
    (* notl and negl both take r/m *)
    | Unary { op; target } -> is_reg_or_mem target
    | Binary { op; left; dst } -> validate_binary op left dst
    | Idivl { divisor } -> is_reg_or_mem divisor
    | AllocateStack { size } -> size >= 0

  let validate_instruction_with_reporting (fn_name : string)
      (inst : instruction) : bool =
    let result = validate_instruction inst in
    if not result then begin
      prerr_string @@ "Invalid instruction in function " ^ fn_name ^ ":\n";
      prerr_string @@ Emit.string_of_instruction inst
    end;
    result

  let validate (Programme { entry }) =
    match entry with
    | Function { name; insts } ->
        let results =
          List.map (validate_instruction_with_reporting name) insts
        in
        List.for_all Fun.id results
end
