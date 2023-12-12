open! Core
(* open Import *)

type t

val create: unit -> t
val add_state : t -> State.t -> [`Duplicate of int | `Ok of int]
val add_edge : t -> int -> string -> int -> [ `Duplicate | `Ok ]
val get : t -> int -> string -> int option
val to_string_hum : t -> string
