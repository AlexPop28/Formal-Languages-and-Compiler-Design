open! Core

let scan =
  let operators = "\\+|\\-|\\*|/|%|==|<=|<|>=|>|=|!=" in
  let separators = "[{}(); ]|$" in
  let append_operator_or_separator pattern =
    pattern ^ "(" ^ operators ^ "|" ^ separators ^ ")"
  in
  let reserved_words =
    "^(int|str|double|if|else|while|get|set|read_int|read_str|read_double|print_int|print_str|print_double)"
    |> append_operator_or_separator
  in
  let reserved_words = [ Re2.create_exn reserved_words ] in
  let operators = [ Re2.create_exn ("^(" ^ operators ^ ")") ] in
  let separators = [ Re2.create_exn ("^(" ^ separators ^ ")") ] in
  let int_constant = "^(0|[+-]?[1-9][0-9]*)" in
  let double_constant = "^(0|[+-]?[1-9][0-9]*(\\.[0-9])?)" in
  let str_constant = "^(\"[^\"]*\")" in
  let constants =
    List.map
      ~f:(fun s -> append_operator_or_separator s |> Re2.create_exn)
      [ int_constant; double_constant; str_constant ]
  in
  let identifiers = "^([a-z][a-z0-9_]*)" |> append_operator_or_separator in
  let identifiers = [ Re2.create_exn identifiers ] in
  Scanner.scan ~separators ~operators ~reserved_words ~constants ~identifiers

let command =
  Command.basic_or_error ~summary:"Run lexical analysis on a source code file"
    (let%map_open.Command file = anon ("filename" %: Filename_unix.arg_type) in
     fun () ->
       let program = In_channel.read_all file in
       let%map.Or_error st, _pif = scan ~program in
       Out_channel.write_all "st.out" ~data:(Symbol_table.to_hum st))

let () = Command_unix.run command
