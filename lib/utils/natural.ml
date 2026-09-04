open! Core

type t = int [@@deriving equal, sexp]

let create n = if n >= 0 then Ok n else error_s [%message "" (n : int)]
let create_exn n = Or_error.ok_exn (create n)
let zero = 0
let is_zero t = t = 0
let to_int t = t
let ( + ) n1 n2 = to_int n1 + to_int n2
let ( - ) n1 n2 = create_exn (to_int n1 - to_int n2)
let negate n = to_int (-n)
