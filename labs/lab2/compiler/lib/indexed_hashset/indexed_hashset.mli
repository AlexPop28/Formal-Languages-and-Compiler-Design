open! Core

type t

val create : unit -> t
val add : t -> string -> int
val get_by_index : t -> int -> string option
(* val get_index : t -> key:string -> 'value option *)
(* val iter : 'value t -> f:(key:string -> value:'value -> unit) -> unit *)
