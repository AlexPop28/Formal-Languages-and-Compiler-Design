open! Core
open Import

module Tokens_data = struct
  type t = {
    operators : string list;
    separators : string list;
    reserved_words : string list;
  }
  [@@deriving sexp]
  (** Plain text representation of operators, separators and reserved words. End of line separators denoted as $. *)
end

let detect_token ~separators ~operators ~reserved_words ~constants ~identifiers
    ~program ~line_number =
  let does_match regex = Re2.matches regex program in
  let get_match regex =
    (Re2.find_submatches_exn regex program).(1) |> Option.value_exn
  in
  match does_match reserved_words with
  | true -> get_match reserved_words |> Token.Reserved_word |> Ok
  | false -> (
      match does_match separators with
      | true -> get_match separators |> Token.Separator |> Ok
      | false -> (
          match does_match operators with
          | true -> get_match operators |> Token.Operator |> Ok
          | false -> (
              match List.find constants ~f:does_match with
              | Some regex -> get_match regex |> Token.Constant |> Ok
              | None -> (
                  match List.find identifiers ~f:does_match with
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

let scan_with_tokens_data ~constants ~identifiers ~(tokens_data : Tokens_data.t)
    ~program =
  let wrap_each_char =
    List.map ~f:(fun s ->
        let wrapped = ref "" in
        String.iter s ~f:(fun c ->
            if Char.(c = '$') then wrapped := String.of_char c
            else wrapped := !wrapped ^ "[" ^ String.of_char c ^ "]");
        !wrapped)
  in
  let reduce = List.reduce_exn ~f:(fun acc op -> acc ^ "|" ^ op) in

  let operators = tokens_data.operators |> wrap_each_char |> reduce in

  let separators = tokens_data.separators |> wrap_each_char |> reduce in
  let append_operator_or_separator pattern =
    pattern ^ "(" ^ operators ^ "|" ^ separators ^ ")"
  in
  let reserved_words =
    "^(" ^ reduce tokens_data.reserved_words ^ ")"
    |> append_operator_or_separator
  in
  let reserved_words = Re2.create_exn reserved_words in
  let operators = Re2.create_exn ("^(" ^ operators ^ ")") in
  let separators = Re2.create_exn ("^(" ^ separators ^ ")") in
  let append_operator_or_separator_and_compile s =
    append_operator_or_separator s |> Re2.create_exn
  in

  let constants =
    List.map ~f:append_operator_or_separator_and_compile constants
  in
  let identifiers =
    List.map identifiers ~f:append_operator_or_separator_and_compile
  in
  scan ~separators ~operators ~reserved_words ~constants ~identifiers ~program
