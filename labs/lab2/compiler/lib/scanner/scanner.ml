open! Core
open Import

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
                          "Lexical error at line %{line_number#Int}: %{program}"]
                  ))))

let scan ~separators ~operators ~reserved_words ~constants ~identifiers ~program
    =
  let st = Symbol_table.create () in
  let pif = Pif.create () in

  let rec process_line line_number line =
    match String.strip line with
    | "" -> Ok ()
    | line ->
        let%bind.Or_error token =
          detect_token ~separators ~operators ~reserved_words ~constants
            ~identifiers ~program:line ~line_number
        in
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
