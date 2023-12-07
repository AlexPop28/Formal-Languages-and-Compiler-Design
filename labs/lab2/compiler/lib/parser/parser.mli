open! Core

type t

module Lr0_item : sig
  type t =
    { lhp : string
    ; left_dot : string list (* list is backwards *)
    ; right_dot : string list
    }
  [@@deriving compare, hash, sexp, equal]
end

module State : sig
  type t = { items : Lr0_item.t Hash_set.t }

  val sexp_of_t : t -> Sexp.t
  val to_string_hum : t -> string
end

val create : Enhanced_grammar.t -> t
val get_cannonical_collection : t -> State.t list Or_error.t
val closure : t -> Lr0_item.t Hash_set.t -> State.t
val goto : t -> State.t -> string -> State.t Or_error.t
