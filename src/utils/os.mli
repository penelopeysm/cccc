(* should probably use intf here... *)
type t =
  | MacOS
  | Linux
  | Unknown
[@@deriving equal]

val current_os : t
