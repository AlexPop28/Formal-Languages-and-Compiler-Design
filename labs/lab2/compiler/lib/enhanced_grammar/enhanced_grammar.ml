open! Core

type t = {
  non_terminals : string list;
  terminals : string list;
  starting_symbol : string;
  productions : (string * string list) list;
}

let create grammar =
  if not (Grammar.is_context_free grammar) then
    Or_error.error_s
      [%message "Passed grammar is not context-free" (grammar : Grammar.t)]
  else
    let productions =
      List.map grammar.productions ~f:(fun (lhs, rhs) -> (List.hd_exn lhs, rhs))
    in
    Ok
      {
        non_terminals = "S'" :: grammar.non_terminals;
        terminals = grammar.terminals;
        starting_symbol = "S'";
        productions = ("S'", [ grammar.starting_symbol ]) :: productions;
      }
