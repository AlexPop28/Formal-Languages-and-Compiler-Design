open! Core

type t =
  { non_terminals : string list
  ; terminals : string list
  ; starting_symbol : string
  ; productions : (string list * string list) list
  }
[@@deriving sexp]

let validate_side_has_only_declared_symbols t side =
  List.map side ~f:(fun symbol ->
    if List.mem (t.non_terminals @ t.terminals) symbol ~equal:String.equal
    then Ok ()
    else Or_error.error_s [%message (symbol : string) " is undeclared"])
  |> Or_error.all
  |> Or_error.map ~f:(fun _ -> ())
;;

let validate_production_has_only_declared_symbols t (lhs, rhs) =
  [ validate_side_has_only_declared_symbols t lhs
  ; validate_side_has_only_declared_symbols t rhs
  ]
  |> Or_error.all
  |> Or_error.map ~f:(fun _ -> ())
;;

let validate_symbols_are_declared t =
  let%bind.Or_error _ =
    List.map t.productions ~f:(validate_production_has_only_declared_symbols t)
    |> Or_error.all
    |> Or_error.map ~f:(fun _ -> ())
  in
  validate_side_has_only_declared_symbols t [ t.starting_symbol ]
;;

let validate_all_lhs_have_nonterminal t =
  List.map t.productions ~f:(fun (lhs, rhs) ->
    if List.exists lhs ~f:(List.mem t.non_terminals ~equal:String.equal)
    then Ok ()
    else
      Or_error.error_s
        [%message
          ((lhs, rhs) : string list * string list)
            " does not have nonterminal on the left"])
  |> Or_error.all
  |> Or_error.map ~f:(fun _ -> ())
;;

let validate_all_nonterminals_appear_in_lhs t =
  List.map t.non_terminals ~f:(fun nonterminal ->
    if List.exists t.productions ~f:(fun (lhs, _) ->
         List.mem lhs nonterminal ~equal:String.equal)
    then Ok ()
    else
      Or_error.error_s [%message (nonterminal : string) " does not have any production"])
  |> Or_error.all
  |> Or_error.map ~f:(fun _ -> ())
;;

let validate_all_symbols_except_source_appear_in_rhs t =
  List.map (t.terminals @ t.non_terminals) ~f:(fun symbol ->
    if String.equal symbol t.starting_symbol
       || List.exists t.productions ~f:(fun (_, rhs) ->
         List.mem rhs symbol ~equal:String.equal)
    then Ok ()
    else Or_error.error_s [%message (symbol : string) " cannot be obtained"])
  |> Or_error.all
  |> Or_error.map ~f:(fun _ -> ())
;;

let validate t =
  [ validate_symbols_are_declared t
  ; validate_all_lhs_have_nonterminal t
  ; validate_all_nonterminals_appear_in_lhs t
  ; validate_all_symbols_except_source_appear_in_rhs t
  ]
  |> Or_error.all
  |> Or_error.map ~f:(fun _ -> t)
;;

let create ~non_terminals ~terminals ~starting_symbol ~productions =
  { non_terminals; terminals; starting_symbol; productions } |> validate
;;

let create_from_file filename =
  In_channel.read_all filename |> Sexp.of_string |> t_of_sexp |> validate
;;

let is_context_free t =
  List.for_all t.productions ~f:(fun (lhs, _) -> List.length lhs = 1)
;;

let get_non_terminals t = t.non_terminals
let get_terminals t = t.terminals
let get_starting_symbol t = t.starting_symbol
let get_productions t = t.productions

let get_productions_of t lhs =
  let%bind.Or_error () = validate_side_has_only_declared_symbols t lhs in
  List.filter t.productions ~f:(fun (lhp, _) -> List.equal String.equal lhp lhs) |> Ok
;;
