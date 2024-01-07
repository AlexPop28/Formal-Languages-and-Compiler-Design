open! Core

module Parsed_data = struct
  type t =
    { alphabet : char list
    ; states : char list
    ; initial_state : char
    ; final_states : char list
    ; (* from, to, character *)
      transitions : (char * char * char list) list
    }
  [@@deriving sexp]
end

type t =
  { parsed_data : Parsed_data.t
  ; transitions : (char * char, char) Hashtbl.t
  ; is_dfa : bool
  }

let list_exists_or_error l error ~f =
  match List.exists l ~f with
  | true -> Ok ()
  | false -> error
;;

let check_valid_state state states =
  list_exists_or_error
    states
    ~f:(Char.equal state)
    (Or_error.error_s
       [%message (state : char) "is not a valid state" (states : char list)])
;;

let check_in_alphabet symbol alphabet =
  list_exists_or_error
    alphabet
    ~f:(Char.equal symbol)
    (Or_error.error_s
       [%message
         (symbol : char) "is not a valid symbol from the alphabet" (alphabet : char list)])
;;

let t_of_sexp sexp =
  let data = Parsed_data.t_of_sexp sexp in
  let transitions =
    Hashtbl.create
      (module struct
        type t = char * char [@@deriving hash, compare, sexp]
      end)
  in
  let%bind.Or_error is_dfa =
    List.fold_result
      data.transitions
      ~init:true
      ~f:(fun is_dfa (state0, state1, symbols) ->
        let%bind.Or_error () = check_valid_state state0 data.states in
        let%bind.Or_error () = check_valid_state state1 data.states in
        let%bind.Or_error _ =
          Or_error.all
            (List.map symbols ~f:(fun symbol -> check_in_alphabet symbol data.alphabet))
        in
        List.fold symbols ~init:is_dfa ~f:(fun is_dfa symbol ->
          match Hashtbl.add transitions ~key:(state0, symbol) ~data:state1 with
          | `Duplicate -> false
          | `Ok -> is_dfa)
        |> Ok)
  in
  Ok { parsed_data = data; transitions; is_dfa }
;;

let sexp_of_t t = Parsed_data.sexp_of_t t.parsed_data
let get_states t = t.parsed_data.states
let get_alphabet t = t.parsed_data.alphabet
let get_initial_state t = t.parsed_data.initial_state
let get_final_states t = t.parsed_data.final_states
let get_transitions t = t.parsed_data.transitions

let is_final_state t state =
  (* print_s [%message (state : char) (t.parsed_data.final_states : char list)]; *)
  List.exists t.parsed_data.final_states ~f:(Char.equal state)
;;

let get_longest_accepted_prefix_exn t s =
  if not t.is_dfa then raise_s [%message "The automaton is a NFA."];
  let rec dfs state s index =
    let result =
      if index >= String.length s
      then None
      else (
        let%bind.Option next_state =
          Hashtbl.find t.transitions (state, String.get s index)
        in
        dfs next_state s (index + 1))
    in
    match result with
    | Some _ -> result
    | None -> if is_final_state t state then Some index else None
  in
  let%bind.Option index = dfs t.parsed_data.initial_state s 0 in
  Some (String.prefix s index)
;;

let does_accept_exn t s =
  match get_longest_accepted_prefix_exn t s with
  | Some len when String.equal s len -> true
  | _ -> false
;;
