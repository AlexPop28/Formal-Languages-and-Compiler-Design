open! Core

type t =
  { non_terminals : string list
  ; terminals : string list
  ; starting_symbol : string
  ; productions : (string * string list) array
  }

let create grammar =
  if not (Grammar.is_context_free grammar)
  then
    Or_error.error_s [%message "Passed grammar is not context-free" (grammar : Grammar.t)]
  else (
    let productions =
      List.map grammar.productions ~f:(fun (lhs, rhs) -> List.hd_exn lhs, rhs)
      |> List.to_array
    in
    Ok
      { non_terminals = "S'" :: grammar.non_terminals
      ; terminals = grammar.terminals
      ; starting_symbol = "S'"
      ; productions = Array.append [| "S'", [ grammar.starting_symbol ] |] productions
      })
;;

let is_non_terminal t symbol = List.mem t.non_terminals symbol ~equal:String.equal

let get_productions_of t symbol =
  Array.filter_map t.productions ~f:(fun (lhp, rhp) ->
    Option.some_if (String.equal lhp symbol) rhp)
  |> List.of_array
;;

let get_production_by_index t index = t.productions.(index)

let get_index_of_production t (lhp1, rhp1) =
  let res =
    Array.findi t.productions ~f:(fun _ (lhp2, rhp2) ->
      String.(lhp1 = lhp2) && List.equal String.( = ) rhp1 rhp2)
  in
  match res with
  | None ->
    Or_error.error_s
      [%message
        "Invalid production"
          (t.productions : (string * string list) array)
          lhp1
          (rhp1 : string list)]
  | Some (i, _) -> Ok i
;;
