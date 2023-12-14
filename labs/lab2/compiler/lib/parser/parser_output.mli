open! Core

type t

val create : Enhanced_grammar.t -> int list -> t Or_error.t
val to_string : t -> string
