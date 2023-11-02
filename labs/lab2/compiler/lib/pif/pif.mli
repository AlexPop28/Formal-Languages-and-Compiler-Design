open! Core

type t [@@deriving sexp]

val create : unit -> t
val add : t -> token:string -> st_pos:int -> unit
