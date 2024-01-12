open! Core

module Parsed_data : sig
  type t =
    { alphabet : char list
    ; states : char list
    ; initial_state : char
    ; final_states : char list
    ; (* from, to, characters *)
      transitions : (char * char * char list) list
    }
  [@@deriving sexp]
end

type t

val t_of_sexp : Sexplib.Sexp.t -> t Or_error.t
val sexp_of_t : t -> Sexplib.Sexp.t
val get_states : t -> char list
val get_alphabet : t -> char list
val get_initial_state : t -> char
val get_final_states : t -> char list
val get_transitions : t -> (char * char * char list) list

(** Only works for DFAs. For NFAs an exception is raised. *)
val does_accept_exn : t -> string -> bool

(** Only works for DFAs. For NFAs an exception is raised. Returns [None] if there
    is no prefix accepted. This is to account for the fact that the empty prefix
    may not necessarily be accepted. *)
val get_longest_accepted_prefix_exn : t -> string -> string option
