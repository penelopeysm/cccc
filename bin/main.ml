open Climate
open Cccc

let print_with_box (styles : ANSITerminal.style list) (title : string)
    (content : string) : unit =
  let styles_with_bold =
    if List.mem ANSITerminal.Bold styles then styles
    else ANSITerminal.Bold :: styles
  in
  let content =
    let n = String.length content in
    if n > 0 && content.[n - 1] = '\n' then String.sub content 0 (n - 1)
    else content
  in
  let content_lines = String.split_on_char '\n' content in
  let inner_width =
    List.fold_left max (String.length title)
      (List.map String.length content_lines)
  in
  let hbar =
    let buf = Buffer.create ((inner_width + 2) * 3) in
    for _ = 1 to inner_width + 2 do
      Buffer.add_string buf "\xe2\x94\x80"
    done;
    Buffer.contents buf
  in
  let pad s = s ^ String.make (inner_width - String.length s) ' ' in
  let print_line s =
    ANSITerminal.prerr_string styles s;
    prerr_newline ()
  in
  print_line ("\xe2\x94\x8c" ^ hbar ^ "\xe2\x94\x90");
  ANSITerminal.prerr_string styles "\xe2\x94\x82 ";
  ANSITerminal.prerr_string styles_with_bold (pad title);
  print_line " \xe2\x94\x82";
  print_line ("\xe2\x94\x9c" ^ hbar ^ "\xe2\x94\xa4");
  List.iter
    (fun line -> print_line ("\xe2\x94\x82 " ^ pad line ^ " \xe2\x94\x82"))
    content_lines;
  print_line ("\xe2\x94\x94" ^ hbar ^ "\xe2\x94\x98")

let main ~(input_fname : string) ~(output_fname : string option)
    ~(dump_ast : bool) ~(dump_ir : bool) ~(parse : bool) ~(tacky : bool) : unit
    =
  In_channel.with_open_text input_fname (fun inx ->
      let lexbuf = Lexing.from_channel inx in
      let ast = Parser.programme Lexer.read lexbuf in
      if dump_ast then begin
        let buf = Ast.Pp.pp ast in
        print_with_box
          [ ANSITerminal.Foreground ANSITerminal.Green ]
          "AST" (Buffer.contents buf)
      end;
      if parse then exit 0;

      let ir = Ir_gen.ir_of_ast ast in
      if dump_ir then begin
        let buf = Ir.Pp.pp ir in
        print_with_box
          [ ANSITerminal.Foreground ANSITerminal.Blue ]
          "IR" (Buffer.contents buf)
      end;
      if tacky then exit 0;

      let asm = Asm_gen.asm_of_ir ir in
      let asm_text = Emit.string_of_asm asm in
      match output_fname with
      | Some fname ->
          Out_channel.with_open_text fname (fun out_channel ->
              Out_channel.output_string out_channel asm_text)
      | None ->
          print_string asm_text;
          exit 0)

let () =
  let command =
    Command.singleton ~doc:"cccc: c compiler compiling c"
    @@
    let open Arg_parser in
    let+ input_fname = pos_req 0 string ~doc:"Path to the input .c or .i file"
    and+ output_fname =
      named_opt [ "o" ] string
        ~doc:
          "Path to the output file. If not provided, output will be printed to \
           stdout"
    and+ dump_ast =
      flag [ "a"; "dump-ast" ] ~doc:"Additionally dump the AST to stderr"
    and+ dump_ir =
      flag [ "i"; "dump-ir" ] ~doc:"Additionally dump the IR to stderr"
    and+ parse =
      flag [ "parse" ]
        ~doc:"Only perform parsing and exit silently (for testing purposes)"
    and+ tacky =
      flag [ "tacky" ]
        ~doc:
          "Only perform IR generation and exit silently (for testing purposes)"
    in
    main ~input_fname ~output_fname ~dump_ast ~dump_ir ~parse ~tacky
  in
  Command.run command
