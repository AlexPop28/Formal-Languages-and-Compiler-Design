open! Core

module Lr0_item = struct
  type t =
    { lhp : string
    ; left_dot : string list (* list is backwards *)
    ; right_dot : string list
    }
  [@@deriving compare, hash, sexp, equal]

  let shift t =
    match t.right_dot with
    | [] -> Or_error.error_string "Error tried to shift empty righthandside"
    | right_first :: rest ->
      Ok { lhp = t.lhp; left_dot = right_first :: t.left_dot; right_dot = rest }
  ;;

  let create_from_production lhp rhp = { lhp; left_dot = []; right_dot = rhp }

  let to_string_hum t =
    let left_dot = List.rev t.left_dot |> String.concat ~sep:" " in
    let right_dot = String.concat t.right_dot ~sep:" " in
    [%string "[%{t.lhp} -> %{left_dot}.%{right_dot}]"]
  ;;
end

module State = struct
  type t = { items : Lr0_item.t Hash_set.t }

  module Action = struct
    type t =
      | Shift
      | Reduce of Lr0_item.t
      | Accept
    [@@deriving sexp]
  end

  let get_all_lr0_items_right_dot_starting_with_symbol t symbol =
    Hash_set.filter t.items ~f:(fun item ->
      match item.right_dot with
      | hd :: _ when String.equal hd symbol -> true
      | _ -> false)
  ;;

  let equal t1 t2 = Hash_set.equal t1.items t2.items
  let sexp_of_t t = [%sexp (Hash_set.to_list t.items : Lr0_item.t list)]

  let to_string_hum t =
    Hash_set.to_list t.items
    |> List.map ~f:Lr0_item.to_string_hum
    |> String.concat ~sep:" ; "
  ;;

  let get_action t (grammar : Enhanced_grammar.t) =
    let action_list = ref [] in
    if Hash_set.find t.items ~f:(fun lr0_item -> not (List.is_empty lr0_item.right_dot))
       |> Option.is_some
    then action_list := [ Shift ] @ !action_list;
    Hash_set.iter t.items ~f:(fun lr0_item ->
      if List.is_empty lr0_item.right_dot
      then action_list := [ Reduce lr0_item ] @ !action_list);
    let start = grammar.starting_symbol in
    if Hash_set.find t.items ~f:(fun lr0_item ->
         String.( = ) start lr0_item.lhp && List.is_empty lr0_item.right_dot)
       |> Option.is_some
    then action_list := [ Accept ];
    match !action_list with
    | [ action ] -> Ok action
    | action_list -> Error action_list
  ;;
end

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

let get_cannonical_collection t =
  let s0 =
    Enhanced_grammar.get_productions_of t.grammar t.grammar.starting_symbol
    |> List.map ~f:(Lr0_item.create_from_production t.grammar.starting_symbol)
    |> Hash_set.of_list (module Lr0_item)
    |> closure t
  in
  let symbols = t.grammar.terminals @ t.grammar.non_terminals in
  let state_number = ref 0 in
  let cannonical_collection = ref [] in
  let rec dfs state =
    let current_state_number = !state_number in
    cannonical_collection := (state, !state_number) :: !cannonical_collection;
    state_number := !state_number + 1;
    let%bind.Or_error _ =
      List.map symbols ~f:(fun symbol ->
        let%bind.Or_error next_state = goto t state symbol in

        if next state is empty then Ok ()
        else (

        )







        if Hash_set.is_empty next_state.items then Ok ()
        else (
          let id =
            if
           List.mem !cannonical_collection ~equal:(fun (s, _) -> State.equal s next_state)
        then Ok ()
        else dfs next_state)
      |> Or_error.all
    in
    Ok ()
  in
  let%bind.Or_error () = dfs s0 in
  Ok !cannonical_collection
;;
