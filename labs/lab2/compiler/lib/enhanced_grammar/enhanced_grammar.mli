open! Core

type t =
  { non_terminals : string list
  ; terminals : string list
  ; starting_symbol : string
  ; productions : (string * string list) array
  }

(** Returns [Error.t] if the grammar is not context free *)
val create : Grammar.t -> t Or_error.t

val is_non_terminal : t -> string -> bool
val get_productions_of : t -> string -> string list list
val get_production_by_index : t -> int -> string * string list
val get_index_of_production : t -> string * string list -> int Or_error.t
val is_symbol_part_of_grammar: t -> string -> bool 
