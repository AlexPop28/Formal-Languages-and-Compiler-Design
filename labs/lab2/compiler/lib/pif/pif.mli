open! Core

module Token : sig
  type t =
    | Reserved_word of string
    | Separator of string
    | Operator of string
    | Constant of string
    | Identifier of string
  [@@deriving sexp]
end

type t [@@deriving sexp]

val create : unit -> t
val add : t -> token:string -> st_pos:int -> unit
val to_list : t -> (string * int) list
val to_hum : t -> string
val from_hum : string -> t Or_error.t
