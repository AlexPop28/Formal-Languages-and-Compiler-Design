type t

val create : unit -> t
val add_symbol : t -> string -> int
val get_symbol : t -> int -> string option
