(** Natural numbers, i.e., 0, 1, 2, ... *)

open! Core

type t [@@deriving equal, sexp]

val create : int -> t Or_error.t
val create_exn : int -> t
val zero : t
val is_zero : t -> bool
val to_int : t -> int

(* Addition of natural numbers is total. *)
val ( + ) : t -> t -> t

(* Other operations can throw *)
val ( - ) : t -> t -> t
val negate : t -> int
