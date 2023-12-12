open! Core
(* open Import *)

type t =
  { mutable states : (State.t * int) list
  ; mutable next_state_id : int
  ; edges : (int * string, int) Hashtbl.t
  }

let add_state t state =
  match List.find t.states ~f:(fun (s, _) -> State.equal state s) with
  | Some (_, id) -> id
  | None ->
    let result = t.next_state_id in
    t.next_state_id <- t.next_state_id + 1;
    t.states <- (state, result) :: t.states;
    result
;;

let add_edge t state0 symbol state1 =
  Hashtbl.add t.edges ~key:(state0, symbol) ~data:state1
;;

let get t state symbol = Hashtbl.find t.edges (state, symbol)

let to_string_hum t =
  String.concat_lines
    (List.map t.states ~f:(fun (s, id) -> Int.to_string id ^ State.to_string_hum s))
;;
