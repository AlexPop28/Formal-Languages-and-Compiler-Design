open! Core

type t = (string * int option * int option) array

val create : Enhanced_grammar.t -> int list -> t Or_error.t
val to_string : t -> string
