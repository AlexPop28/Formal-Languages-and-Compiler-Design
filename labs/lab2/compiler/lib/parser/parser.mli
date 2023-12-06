open! Core

type t

module Lr0_item : sig
  type t =
    { lhp : string
    ; left_dot : string list (* list is backwards *)
    ; right_dot : string list
    }
end

module State : sig 
  type t = { items : Lr0_item.t Hash_set.t }
end

val create : Enhanced_grammar.t -> t
val get_cannonical_collection : t -> State.t list Or_error.t
