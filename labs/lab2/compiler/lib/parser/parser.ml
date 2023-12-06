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
end

module State = struct
  type t = { items : Lr0_item.t Hash_set.t }

  let get_all_lr0_items_right_dot_starting_with_symbol t symbol =
    Hash_set.filter t.items ~f:(fun item ->
      Option.map (List.hd item.right_dot) ~f:(String.equal symbol) |> Option.is_some)
  ;;

  let equal t1 t2 = Hash_set.equal t1.items t2.items
end

(* canonical collection : State.t array : unde?

   val closure : t -> Lr0_item.t Hash_set.t -> State.t
   val goto : State.t -> string -> State.t
   val canonical_collection : Enhanced_grammar.t -> State.t Hashset.t
*)

type t = { grammar : Enhanced_grammar.t }

let create grammar = { grammar }

let closure t (items : Lr0_item.t Hash_set.t) : State.t =
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

let goto t state symbol =
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
  let cannonical_collection = ref [] in
  let rec dfs state =
    cannonical_collection := state :: !cannonical_collection;
    let%bind.Or_error _ =
      List.map symbols ~f:(fun symbol ->
        let%bind.Or_error next_state = goto t state symbol in
        if not (List.mem !cannonical_collection ~equal:State.equal next_state)
        then dfs next_state
        else Ok ())
      |> Or_error.all
    in
    Ok ()
  in
  let%bind.Or_error () = dfs s0 in
  Ok !cannonical_collection
;;
