open! Core

module Parsed_data : sig
  type t = {
    alphabet : char list;
    states : char list;
    initial_state : char;
    final_states : char list;
    (* from, to, character *)
    transitions : (char * char * char) list;
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
val get_transitions : t -> (char * char * char) list

(* Only works for DFAs. For NFAs an exception is raised . *)
val does_accept_exn : t -> string -> bool
