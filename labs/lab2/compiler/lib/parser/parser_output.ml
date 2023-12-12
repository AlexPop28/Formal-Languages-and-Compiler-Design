open! Core

type t = (string * int option * int option) array

let create grammar output_band =
  let rev_output_band = ref (List.rev output_band) in
  let result : (string * int option * int option) array ref = ref [||] in
  let rec interpret_output_band symbol index =
    let _, last_child, _ = Array.get !result index in
    let last_child = ref last_child in
    if Enhanced_grammar.is_non_terminal grammar symbol
    then (
      match !rev_output_band with
      | [] -> Ok ()
      | production_idx :: rest ->
        rev_output_band := rest;
        let production_from, production_to =
          Array.get grammar.productions production_idx
        in
        if not (String.equal production_from symbol)
        then Or_error.error_string "Production doesn't match index"
        else (
          let symbols_with_indexes =
            List.map production_to ~f:(fun symbol ->
              result := Array.append !result [| symbol, None, None |];
              symbol, Array.length !result - 1)
          in
          List.iter symbols_with_indexes ~f:(fun (_, next_index) ->
            (match !last_child with
             | None ->
               let symbol, _, next_sibling = Array.get !result index in
               Array.set !result index (symbol, Option.some next_index, next_sibling)
             | Some last_child_index ->
               let symbol, first_child, _ = Array.get !result last_child_index in
               Array.set
                 !result
                 last_child_index
                 (symbol, first_child, Option.some next_index));
            last_child := Option.some next_index);
          List.rev symbols_with_indexes
          |> List.map ~f:(fun (next_symbol, next_index) : unit Or_error.t ->
            interpret_output_band next_symbol next_index)
          |> Or_error.all
          |> Or_error.map ~f:(fun _ -> ())))
    else Ok ()
  in
  let unenhanced_grammar_starting_symbol =
    Enhanced_grammar.get_productions_of grammar grammar.starting_symbol
    |> List.hd_exn
    |> List.hd_exn
  in
  result := [| unenhanced_grammar_starting_symbol, None, None |];
  let%bind.Or_error _ = interpret_output_band unenhanced_grammar_starting_symbol 0 in
  Ok !result
;;

let to_string t =
  Array.foldi t ~init:"" ~f:(fun i acc (symbol, first_child, next_sibling) ->
    let first_child_str =
      match first_child with
      | None -> sprintf "%3s" "-"
      | Some first_child_idx -> sprintf "%3d" first_child_idx
    in
    let next_sibling_str =
      match next_sibling with
      | None -> sprintf "%3s" "-"
      | Some next_sibling_idx -> sprintf "%3d" next_sibling_idx
    in
    let row =
      sprintf "| %3d. | %10s | %s | %s |\n" i symbol first_child_str next_sibling_str
    in
    acc ^ row)
;;
