open Cccc
open Js_of_ocaml

let compile_source (source : string) : string =
  source
  |> Lexing.from_string
  |> Parser.programme Lexer.read
  |> Ir_gen.ir_of_ast
  |> Asm_gen.asm_of_ir
  |> Asm.Emit.string_of_asm
;;

let () = Js.export "compile" compile_source
