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

let main ~(fname : string) ~(retain_assembly : bool) ~(run : bool) : unit =
  (* Get name of preprocessed file *)
  let preproc_fname = replace_extension fname ".i" in
  let preproc_command =
    Printf.sprintf "clang -E -P %s -o %s" fname preproc_fname
  in
  try_compilation_stage preproc_command "Preprocessing" [ preproc_fname ];

  (* Run our own compiler! *)
  let assembly_fname = replace_extension fname ".s" in
  (* let compiler_command = (Printf.sprintf "clang -S -O %s -o %s" preproc_fname assembly_fname) in *)
  let compiler_command =
    Printf.sprintf "dune exec cccc -- %s -o %s" preproc_fname assembly_fname
  in
  try_compilation_stage compiler_command "Compilation"
    [ preproc_fname; assembly_fname ];

  (* Run the assembler and linker in one shot *)
  let executable_fname = Filename.remove_extension fname in
  let link_command =
    Printf.sprintf "clang %s -o %s" assembly_fname executable_fname
  in
  try_compilation_stage link_command "Assembling and linking"
    [ preproc_fname; assembly_fname; executable_fname ];

  (* Clean up intermediate files *)
  remove_if_exists preproc_fname;
  if not retain_assembly then remove_if_exists assembly_fname;

  (* Optionally run the executable *)
  if run then exit (Sys.command executable_fname) else exit 0

let () =
  let command =
    Command.singleton ~doc:"ccccd: driver for cccc"
    @@
    let open Arg_parser in
    let+ fname = pos_req 0 string ~doc:"Path to the input C file"
    and+ retain_assembly =
      flag [ "a" ] ~doc:"Retain the generated assembly file"
    and+ run = flag [ "r" ] ~doc:"Run the executable after compilation" in
    main ~fname ~retain_assembly ~run
  in
  Command.run command
