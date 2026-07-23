open Climate

let print_usage_and_exit (exec_name : string) =
  Printf.eprintf "Usage: %s <file.c>\n" (Filename.basename exec_name);
  exit 1

let remove_if_exists (fname : string) : unit =
  if Sys.file_exists fname then Sys.remove fname

let replace_extension (filename : string) (new_ext : string) : string =
  Filename.remove_extension filename ^ new_ext

let try_compilation_stage (command : string) (stage_name : string)
    (cleanup : string list) : unit =
  let exit_code = Sys.command command in
  if exit_code <> 0 then begin
    Printf.eprintf "Error: %s failed with exit code %d\n" stage_name exit_code;
    List.iter remove_if_exists cleanup;
    exit exit_code
  end

let is_arm_mac () : bool =
  let inp = Unix.open_process_in "arch 2>/dev/null" in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  String.trim r = "arm64"

let main ~(fname : string) ~(retain_assembly : bool) ~(run : bool)
    ~(dump_ast : bool) ~(parse : bool) ~(tacky : bool) : unit =
  (* Get name of preprocessed file *)
  let preproc_fname = replace_extension fname ".i" in
  let preproc_command =
    Printf.sprintf "clang -E -P %s -o %s" (Filename.quote fname)
      (Filename.quote preproc_fname)
  in
  try_compilation_stage preproc_command "Preprocessing" [ preproc_fname ];

  let is_arm = is_arm_mac () in

  (* Run our own compiler! *)
  let assembly_fname = replace_extension fname ".s" in

  (* let compiler_command = (Printf.sprintf "clang -S -O %s -o %s" preproc_fname assembly_fname) in *)
  let extra_flags = [] in
  let extra_flags =
    if dump_ast then extra_flags @ [ "--dump-ast" ] else extra_flags
  in
  let extra_flags =
    if parse then extra_flags @ [ "--parse" ] else extra_flags
  in
  let extra_flags =
    if tacky then extra_flags @ [ "--tacky" ] else extra_flags
  in
  let extra_flags_string = String.concat " " extra_flags in

  let compiler_command =
    Printf.sprintf "dune exec cccc -- %s -o %s %s"
      (Filename.quote preproc_fname)
      (Filename.quote assembly_fname)
      extra_flags_string
  in
  try_compilation_stage compiler_command "Compilation"
    [ preproc_fname; assembly_fname ];
  if parse || tacky then exit 0;

  (* Run the assembler and linker in one shot *)
  let executable_fname = Filename.remove_extension fname in
  let link_command = if is_arm then
      Printf.sprintf "clang -arch x86_64 %s -o %s"
        (Filename.quote assembly_fname)
        (Filename.quote executable_fname)
    else
      Printf.sprintf "clang %s -o %s"
        (Filename.quote assembly_fname)
        (Filename.quote executable_fname)
  in
  try_compilation_stage link_command "Assembling and linking"
    [ preproc_fname; assembly_fname; executable_fname ];

  (* Clean up intermediate files *)
  remove_if_exists preproc_fname;
  if not retain_assembly then remove_if_exists assembly_fname;

  (* Optionally run the executable *)
  let run_command =
    if is_arm then Printf.sprintf "arch -x86_64 %s" executable_fname
    else executable_fname
  in
  if run then exit (Sys.command run_command) else exit 0

let () =
  let command =
    Command.singleton ~doc:"ccccd: driver for cccc"
    @@
    let open Arg_parser in
    let+ fname = pos_req 0 string ~doc:"Path to the input C file"
    and+ retain_assembly =
      flag [ "a" ] ~doc:"Retain the generated assembly file"
    and+ run = flag [ "r" ] ~doc:"Run the executable after compilation"
    and+ dump_ast = flag [ "dump-ast" ] ~doc:"Dump the AST to stderr"
    and+ parse =
      flag [ "parse" ]
        ~doc:"Only perform parsing and exit silently (for testing purposes)"
    and+ tacky =
      flag [ "tacky" ]
        ~doc:
          "Only perform IR generation and exit silently (for testing purposes)"
    in
    main ~fname ~retain_assembly ~run ~dump_ast ~parse ~tacky
  in
  Command.run command
