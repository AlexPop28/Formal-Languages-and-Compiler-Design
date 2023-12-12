open! Core

(* open Import *)
module Lr0_item = Lr0_item
module State = State
module Parser_output = Parser_output
module Canonical_collection = Canonical_collection

type t

val create : Enhanced_grammar.t -> t
val closure : t -> Lr0_item.t Hash_set.t -> State.t
val goto : t -> State.t -> string -> State.t Or_error.t

module For_testing : sig
  val get_cannonical_collection : t -> Canonical_collection.t Or_error.t
end
