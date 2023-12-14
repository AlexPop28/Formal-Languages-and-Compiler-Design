open! Core

(* open Import *)
module Lr0_item = Lr0_item
module State = State
module Parser_output = Parser_output
module Parsing_table = Parsing_table

type t

val create : Enhanced_grammar.t -> t
val get_parsing_table : t -> Parsing_table.t Or_error.t
val parse : t -> string list -> Parser_output.t Or_error.t

module For_testing : sig
  val closure : t -> Lr0_item.t Hash_set.t -> State.t
  val goto : t -> State.t -> string -> State.t Or_error.t
  val get_cannonical_collection : t -> (State.t * int) list Or_error.t
end
