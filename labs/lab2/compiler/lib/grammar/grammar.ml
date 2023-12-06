open! Core

type t =
  { non_terminals : string list
  ; terminals : string list
  ; starting_symbol : string
  ; productions : (string list * string list) list
  }
[@@deriving sexp]

let create ~non_terminals ~terminals ~starting_symbol ~productions =
  { non_terminals; terminals; starting_symbol; productions }
;;

let create_from_file ~filename =
  In_channel.read_all filename |> Sexp.of_string |> t_of_sexp
;;

let is_context_free t =
  List.for_all t.productions ~f:(fun (lhs, _) -> List.length lhs = 1)
;;

let get_non_terminals t = t.non_terminals
let get_terminals t = t.terminals
let get_starting_symbol t = t.starting_symbol
let get_productions t = t.productions
