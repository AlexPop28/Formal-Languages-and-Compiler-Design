open! Core

module Token = struct
  type t =
    | Reserved_word of string
    | Separator of string
    | Operator of string
    | Constant of string
    | Identifier of string
  [@@deriving sexp]
end

type t = (string * int) list ref [@@deriving sexp]

let create () = ref []
let add t ~token ~st_pos = t := (token, st_pos) :: !t
let to_list t = List.rev !t
