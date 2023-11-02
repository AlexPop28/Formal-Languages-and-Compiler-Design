open! Core
open Import

(* let detect_token ~separators ~operators ~reserved_words ~constants ~identifiers *)
(*     ~program = *)
(*   let token_categories = *)
(*     [ *)
(*       (reserved_words, "Reserved_word"); *)
(*       (separators, "Separator"); *)
(*       (operators, "Operator"); *)
(*       (constants, "Constant"); *)
(*       (identifiers, "Identifier"); *)
(*     ] *)
(*   in *)

(*   let find_token_category token_category = *)
(*     match *)
(*       List.find token_category ~f:(fun regex -> Re2.matches regex program) *)
(*     with *)
(*     | Some regex -> (Re2.find_submatches_exn regex program).(1) *)
(*     | None -> None *)
(*   in *)

(*   let result = *)
(*     List.find_map token_categories ~f:(fun (category, token_type) -> *)
(*         Option.map (find_token_category category) ~f:(fun match_result -> *)
(*             (token_type, match_result))) *)
(*   in *)

(*   match result with *)
(*   | Some ("Reserved_word", token) -> Ok (Token.Reserved_word token) *)
(*   | Some ("Separator", token) -> Ok (Token.Separator token) *)
(*   | Some ("Operator", token) -> Ok (Token.Operator token) *)
(*   | Some ("Constant", token) -> Ok (Token.Constant token) *)
(*   | Some ("Identifier", token) -> Ok (Token.Identifier token) *)
(*   | _ -> Or_error.error_string "Lexical error" *)

let detect_token ~separators ~operators ~reserved_words ~constants ~identifiers
    ~program ~line_number =
  let f regex = Re2.matches regex program in
  let get_match regex =
    (Re2.find_submatches_exn regex program).(1) |> Option.value_exn
  in
  match List.find reserved_words ~f with
  | Some regex -> get_match regex |> Token.Reserved_word |> Ok
  | None -> (
      match List.find separators ~f with
      | Some regex -> get_match regex |> Token.Separator |> Ok
      | None -> (
          match List.find operators ~f with
          | Some regex -> get_match regex |> Token.Operator |> Ok
          | None -> (
              match List.find constants ~f with
              | Some regex -> get_match regex |> Token.Constant |> Ok
              | None -> (
                  match List.find identifiers ~f with
                  | Some regex -> get_match regex |> Token.Identifier |> Ok
                  | None ->
                      Or_error.error_string
                        [%string
                          "Lexical error at %{line_number#Int}: %{program}"]))))

let scan ~separators ~operators ~reserved_words ~constants ~identifiers ~program
    =
  let st = Symbol_table.create () in
  let pif = Pif.create () in

  (* we sort the operators decreasing by their length to do look
     ahead *)
  let rec process_line line_number line =
    match String.strip line with
    | "" -> Ok ()
    | line ->
        let%bind.Or_error token =
          detect_token ~separators ~operators ~reserved_words ~constants
            ~identifiers ~program:line ~line_number
        in
        (* print_s [%message (line : string) (token : Token.t)]; *)
        let token =
          match (token : Pif.Token.t) with
          | Reserved_word token | Separator token | Operator token ->
              Pif.add pif ~token ~st_pos:(-1);
              token
          | Identifier identifier ->
              let st_pos = Symbol_table.add_symbol st identifier in
              Pif.add pif ~token:"id" ~st_pos;
              identifier
          | Constant constant ->
              let st_pos = Symbol_table.add_symbol st constant in
              Pif.add pif ~token:"const" ~st_pos;
              constant
        in
        process_line line_number (String.drop_prefix line (String.length token))
  in
  let%bind.Or_error () =
    String.split_lines program
    |> List.mapi ~f:(fun i line -> process_line (i + 1) line)
    |> Or_error.all_unit
  in
  Ok (st, pif)
