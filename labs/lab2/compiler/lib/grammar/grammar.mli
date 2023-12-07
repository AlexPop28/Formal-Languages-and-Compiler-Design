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
  -> t Or_error.t

val create_from_file : string -> t Or_error.t
val is_context_free : t -> bool
val get_non_terminals : t -> string list
val get_terminals : t -> string list
val get_starting_symbol : t -> string
val get_productions : t -> (string list * string list) list
val get_productions_of : t -> string list -> (string list * string list) list Or_error.t
