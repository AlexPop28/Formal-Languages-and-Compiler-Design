open! Core

let scan =
  let int_constant = "^(0|[+-]?[1-9][0-9]*)" in
  let double_constant = "^(0|[+-]?[1-9][0-9]*(\\.[0-9])?)" in
  let str_constant = "^(\"[^\"]*\")" in
  let constants = [ int_constant; double_constant; str_constant ] in
  let identifiers = [ "^([a-z][a-z0-9_]*)" ] in
  Scanner.scan_with_tokens_data ~constants ~identifiers

let command =
  Command.basic_or_error ~summary:"Run lexical analysis on a source code file"
    (let%map_open.Command file = anon ("filename" %: Filename_unix.arg_type)
     and tokens_file = anon ("tokens_file" %: Filename_unix.arg_type)
     and st_out = anon ("symbol_table_output_file" %: Filename_unix.arg_type)
     and pif_out =
       anon ("program_internal_form_output_file" %: Filename_unix.arg_type)
     in

     fun () ->
       let program = In_channel.read_all file in
       let tokens_data =
         In_channel.read_all tokens_file
         |> Sexp.of_string |> Scanner.Tokens_data.t_of_sexp
       in
       let%map.Or_error st, pif = scan ~tokens_data ~program in
       Out_channel.write_all st_out ~data:(Symbol_table.to_hum st);
       Out_channel.write_all pif_out ~data:(Pif.to_hum pif);
       print_endline "Lexically correct")

let () = Command_unix.run command
