type t =
  | MacOS
  | Linux
  | Unknown
[@@deriving equal]

let current_os =
  (* Platform.system is defined via a Dune build rule *)
  match Platform.system with
  | "macosx" -> MacOS
  | "linux" -> Linux
  | _ -> Unknown
;;
