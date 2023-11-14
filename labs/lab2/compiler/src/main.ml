open! Core

let scan =
  let int_constant = "^(0|[+-]?[1-9][0-9]*)" in
  let double_constant = "^(0|[+-]?[1-9][0-9]*(\\.[0-9])?)" in
  let str_constant = "^(\"[^\"]*\")" in
  let constants = [ int_constant; double_constant; str_constant ] in
  let identifiers = [ "^([a-z][a-z0-9_]*)" ] in
  Scanner.scan_with_tokens_data ~constants ~identifiers

let scanner_command =
  Command.basic_or_error ~summary:"Run lexical analysis on a source code file"
    (let open Command.Let_syntax in
     let%map_open file = anon ("source-code-file" %: Filename_unix.arg_type)
     and tokens_file =
       flag "tokens" (required Filename_unix.arg_type) ~doc:"FILE Tokens file"
     and st_out =
       flag "st"
         (required Filename_unix.arg_type)
         ~doc:"FILE Symbol table output file"
     and pif_out =
       flag "pif"
         (required Filename_unix.arg_type)
         ~doc:"FILE Program internal form output file"
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

let () = Command_unix.run scanner_command
