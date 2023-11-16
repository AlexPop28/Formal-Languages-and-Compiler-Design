open! Core

module Tokens_data : sig
  type t = {
    operators : string list;
    separators : string list;
    reserved_words : string list;
  }
  [@@deriving sexp]
end

val scan :
  separators:Re2.t ->
  operators:Re2.t ->
  reserved_words:Re2.t ->
  constants:Re2.t list ->
  identifiers:Re2.t list ->
  program:string ->
  (Symbol_table.t * Pif.t) Or_error.t
(** [separators] and [operators] should contain regular expressions that are
    enclosed in a group.

    [reserved_words] should contain regular expressions with two groups, the
    first one being the regexp for the reserved word, and the second one being a
    regular expression for an operator or a separator.

    [constants] and [identifiers] should be constructed in a similar manner to
    [reserved_words]. *)

val scan_with_tokens_data :
  constants:string list ->
  identifiers:string list ->
  tokens_data:Tokens_data.t ->
  program:string ->
  (Symbol_table.t * Pif.t) Or_error.t
(** [constants] and [identifiers] contain string representations of the regular expressions to be enhanced and compiled. *)
