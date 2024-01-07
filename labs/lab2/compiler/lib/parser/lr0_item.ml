open! Core

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
