open! Core
(* open Import *)

type t = { items : Lr0_item.t Hash_set.t }

module Action : sig
  type t =
    | Shift
    | Reduce of (string * string list)
    | Accept
  [@@deriving sexp]
end

val equal : t -> t -> bool
val sexp_of_t : t -> Sexp.t
val to_string_hum : t -> string
val get_action : t -> Enhanced_grammar.t -> Action.t Or_error.t

val get_all_lr0_items_right_dot_starting_with_symbol
  :  t
  -> string
  -> Lr0_item.t Hash_set.t
