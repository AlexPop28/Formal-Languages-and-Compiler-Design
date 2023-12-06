open! Core

type t =
  { non_terminals : string list
  ; terminals : string list
  ; starting_symbol : string
  ; productions : (string list * string list) list
  }
[@@deriving sexp]

val create
  :  non_terminals:string list
  -> terminals:string list
  -> starting_symbol:string
  -> productions:(string list * string list) list
  -> t

val create_from_file : filename:string -> t
val is_context_free : t -> bool
val get_non_terminals : t -> string list
val get_terminals : t -> string list
val get_starting_symbol : t -> string
val get_productions : t -> (string list * string list) list
