open! Core

(* TODO open Import *)
module State = State
module Lr0_item = Lr0_item
module Parser_output = Parser_output
module Canonical_collection = Canonical_collection

type t = { grammar : Enhanced_grammar.t }

let create grammar = { grammar }

let closure_one_step t (items : Lr0_item.t Hash_set.t) : State.t =
  (* print_s [%message "Making closure of:" (Hash_set.to_list items : Lr0_item.t list)]; *)
  { items =
      Hash_set.fold items ~init:(Hash_set.copy items) ~f:(fun acc item ->
        match List.hd item.right_dot with
        | Some non_terminal when Enhanced_grammar.is_non_terminal t.grammar non_terminal
          ->
          List.fold
            ~init:acc
            (Enhanced_grammar.get_productions_of t.grammar non_terminal)
            ~f:(fun acc rhs ->
              Hash_set.add acc { lhp = non_terminal; left_dot = []; right_dot = rhs };
              acc)
        | _ -> acc)
  }
;;

let rec closure t (items : Lr0_item.t Hash_set.t) : State.t =
  let next_step_closure = closure_one_step t items in
  let curr_state : State.t = { items } in
  if State.equal curr_state next_step_closure
  then curr_state
  else closure t next_step_closure.items
;;

let goto t state symbol =
  (* TODO: validate that symbol is in the grammar *)
  let%map.Or_error lr0_items =
    State.get_all_lr0_items_right_dot_starting_with_symbol state symbol
    |> Hash_set.to_list
    |> List.map ~f:Lr0_item.shift
    |> Or_error.all
  in
  Hash_set.of_list (module Lr0_item) lr0_items |> closure t
;;

let get_cannonical_collection_and_parsing_table t =
  let s0 =
    Enhanced_grammar.get_productions_of t.grammar t.grammar.starting_symbol
    |> List.map ~f:(Lr0_item.create_from_production t.grammar.starting_symbol)
    |> Hash_set.of_list (module Lr0_item)
    |> closure t
  in
  let symbols = t.grammar.terminals @ t.grammar.non_terminals in
  let canonical_collection = Canonical_collection.create () in
  let rec dfs state =
    let current_id = (Canonical_collection.add_state canonical_collection state) in
    match current_id with 
    | `Duplicate id -> Ok id
    | `Ok id -> (
      let%bind.Or_error _ =
        List.map symbols ~f:(fun symbol ->
          let%bind.Or_error next_state = goto t state symbol in
          if Hash_set.is_empty next_state.items
          then Ok ()
          else (
            let%bind.Or_error _id = dfs next_state in
            Ok () (* TODO: set goto (current_state_number, symbol) = id) *)))
        |> Or_error.all
      in
      Ok id
    )
  in
  let%bind.Or_error _root_state_number = dfs s0 in
  (* TODO: return the parsing table *)
  Ok (canonical_collection, ())
;;

module For_testing = struct
  let get_cannonical_collection t =
    let%bind.Or_error result = get_cannonical_collection_and_parsing_table t in
    fst result |> Ok
  ;;
end
