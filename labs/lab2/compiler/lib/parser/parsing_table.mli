open! Core
(* open Import *)

type t

val create : Enhanced_grammar.t -> t
val add_state : t -> State.t -> [ `Duplicate of int | `Ok of int ] Or_error.t
val add_edge : t -> int -> string -> int -> [ `Duplicate | `Ok ]
val goto : t -> int -> string -> int Or_error.t
val to_string_hum : t -> string
val build_actions : t -> t Or_error.t
val get_action : t -> int -> State.Action.t Or_error.t
val get_root : t -> int
val set_root : t -> int -> unit

module For_testing : sig
  val get_canonical_collection_string : t -> string
  val get_canonical_collection : t -> (State.t * int) list
end
