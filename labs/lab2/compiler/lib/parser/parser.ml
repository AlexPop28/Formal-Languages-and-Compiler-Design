open! Core

module Lr0_item = struct
  type t = { lhp : string; left_dot : string list; right_dot : string list }
  [@@deriving compare, hash, sexp]
end

module State = struct
  type t = { items : Lr0_item.t Hash_set.t }
end

(* canonical collection : State.t array : unde?

   val closure : t -> Lr0_item.t Hash_set.t -> State.t
   val goto : State.t -> string -> State.t
   val canonical_collection : Enhanced_grammar.t -> State.t Hashset.t
*)

type t = { grammar : Enhanced_grammar.t }

let create grammar = { grammar }

let closure t (items : Lr0_item.t Hash_set.t) =
  Hash_set.fold items ~init:(Hash_set.copy items) ~f:(fun acc item ->
      match List.hd item.right_dot with
      | Some non_terminal
        when Enhanced_grammar.is_non_terminal t.grammar non_terminal ->
          List.fold ~init:acc
            (Enhanced_grammar.get_productions_of t.grammar non_terminal)
            ~f:(fun acc rhs ->
              Hash_set.add acc
                { lhp = non_terminal; left_dot = []; right_dot = rhs };
              acc)
      | _ -> acc)
