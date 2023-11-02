open! Core

type t = (string * int) list ref [@@deriving sexp]

let create () = ref []
let add t ~token ~st_pos = t := (token, st_pos) :: !t
