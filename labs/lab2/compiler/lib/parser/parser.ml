open! Core

(* TODO open Import *)
module State = State
module Lr0_item = Lr0_item
module Parser_output = Parser_output
module Parsing_table = Parsing_table

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

let get_parsing_table t =
  let s0 =
    Enhanced_grammar.get_productions_of t.grammar t.grammar.starting_symbol
    |> List.map ~f:(Lr0_item.create_from_production t.grammar.starting_symbol)
    |> Hash_set.of_list (module Lr0_item)
    |> closure t
  in
  let symbols = t.grammar.terminals @ t.grammar.non_terminals in
  let parsing_table = Parsing_table.create t.grammar in
  let rec dfs state =
    let%bind.Or_error current_id = Parsing_table.add_state parsing_table state in
    match current_id with
    | `Duplicate id -> Ok id
    | `Ok id ->
      let%bind.Or_error _ =
        List.map symbols ~f:(fun symbol ->
          let%bind.Or_error next_state = goto t state symbol in
          if Hash_set.is_empty next_state.items
          then Ok ()
          else (
            let%bind.Or_error child_id = dfs next_state in
            match Parsing_table.add_edge parsing_table id symbol child_id with
            | `Ok -> Ok ()
            | `Duplicate ->
              Or_error.error_s
                [%message
                  "goto of state and symbol computed twice"
                    (State.to_string_hum state)
                    symbol]))
        |> Or_error.all
      in
      Ok id
  in
  let%bind.Or_error root_state_number = dfs s0 in
  Parsing_table.set_root parsing_table root_state_number;
  Ok parsing_table
;;

let rec parse_ t parsing_table work input output =
  match work with
  | [] -> failwith "something went wrong"
  | (_, state) :: _ ->
    let%bind.Or_error action = Parsing_table.get_action parsing_table state in
    (match action, input with
     | Accept, [] -> Ok output
     | Accept, _ ->
       Or_error.error_s
         [%message "Reached accept state but input is not empty" (input : string list)]
     | Shift, hd :: new_input ->
       let%bind.Or_error new_state = Parsing_table.goto parsing_table state hd in
       parse_ t parsing_table ((hd, new_state) :: work) new_input output
     | Shift, [] -> Or_error.error_s [%message "Shifting with empty input" (state : int)]
     | Reduce (lhp, rhp), input ->
       let%bind.Or_error production =
         Enhanced_grammar.get_index_of_production t.grammar (lhp, rhp)
       in
       let prod, rem_work = List.split_n work (List.length rhp) in
       if List.exists2_exn (List.rev prod) rhp ~f:(fun (a, _) b -> String.(a = b))
       then
         Or_error.error_s
           [%message
             "Trying to reduce but the working stack does not contain the right hand \
              side of the production"
               (rhp : string list)
               (work : (string * int) list)]
       else (
         let%bind.Or_error new_state =
           Parsing_table.goto parsing_table (snd (List.hd_exn rem_work)) lhp
         in
         parse_ t parsing_table ((lhp, new_state) :: rem_work) input (production :: output)))
;;

let parse t input =
  let%bind.Or_error parsing_table = get_parsing_table t in
  let%bind.Or_error parsing_table = Parsing_table.build_actions parsing_table in
  let%bind.Or_error output_band =
    parse_ t parsing_table [ "", Parsing_table.get_root parsing_table ] input []
  in
  Parser_output.create t.grammar output_band
;;

module For_testing = struct
  let closure = closure
  let goto = goto
  let get_parsing_table = get_parsing_table

  let get_cannonical_collection t =
    let%bind.Or_error result = get_parsing_table t in
    Parsing_table.For_testing.get_canonical_collection result |> Ok
  ;;
end
