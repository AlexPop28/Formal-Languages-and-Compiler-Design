open! Core

type t = {
  non_terminals : string list;
  terminals : string list;
  starting_symbol : string;
  productions : (string * string list) list;
}

val create : Grammar.t -> t Or_error.t
(** Returns [Error.t] if the grammar is not context free *)

val is_non_terminal : t -> string -> bool
val get_productions_of : t -> string -> string list list
