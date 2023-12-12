open! Core

type t =
  { lhp : string
  ; left_dot : string list (* list is backwards *)
  ; right_dot : string list
  }
[@@deriving compare, hash, sexp, equal]

val create_from_production : string -> string list -> t
val shift : t -> t Or_error.t
val to_string_hum : t -> string
