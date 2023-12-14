open! Core
(* open Import *)

type t =
  { grammar : Enhanced_grammar.t
  ; mutable states : (State.t * int) list
  ; mutable next_state_id : int
  ; goto : (int * string, int) Hashtbl.t
  ; action : (int, State.Action.t) Hashtbl.t
  }

let create grammar =
  { grammar
  ; states = []
  ; next_state_id = 0
  ; goto =
      Hashtbl.create
        (module struct
          type t = int * string [@@deriving hash, sexp, equal, compare]
        end)
  ; action = Hashtbl.create (module Int)
  }
;;

let add_state t state =
  match List.find t.states ~f:(fun (s, _) -> State.equal state s) with
  | Some (_, id) -> `Duplicate id |> Ok
  | None ->
    let id = t.next_state_id in
    t.next_state_id <- t.next_state_id + 1;
    t.states <- (state, id) :: t.states;
    (* TODO: add these lines back after we fix the conflicts and remove the
       [build_actions] function *)
    (* let%bind.Or_error action = State.get_action state t.grammar in *)
    (* Hashtbl.set t.action ~key:id ~data:action; *)
    `Ok id |> Ok
;;

let add_edge t state0 symbol state1 =
  Hashtbl.add t.goto ~key:(state0, symbol) ~data:state1
;;

let goto t state symbol = Hashtbl.find t.goto (state, symbol)

let get_canonical_collection_string t =
  String.concat_lines
    (List.map t.states ~f:(fun (s, id) -> Int.to_string id ^ ": " ^ State.to_string_hum s))
;;

let to_string_hum t = get_canonical_collection_string t

let build_actions t =
  let%bind.Or_error _ =
    List.map t.states ~f:(fun (state, id) ->
      let%map.Or_error action = State.get_action state t.grammar in
      Hashtbl.set t.action ~key:id ~data:action)
    |> Or_error.all
  in
  Ok t
;;

let get_action t state_id =
  match Hashtbl.find t.action state_id with
  | None -> Or_error.error_s [%message "Invalid state number" (state_id : int)]
  | Some action -> Ok action
;;

module For_testing = struct
  let get_canonical_collection_string = get_canonical_collection_string
  let get_canonical_collection t = t.states
end
