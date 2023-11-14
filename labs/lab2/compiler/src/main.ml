open! Core

module Finite_automaton_command = struct
  let automaton_param =
    Command.Param.anon
      (Command.Anons.( %: ) "automaton-file" Filename_unix.arg_type)

  let describe_command =
    Command.basic_or_error ~summary:"Describe the parsed automaton from a file"
      (let%map_open.Command automaton_file = automaton_param
       and alphabet = flag "alphabet" no_arg ~doc:"Print the alphabet"
       and states = flag "states" no_arg ~doc:"Print all the states"
       and initial_state =
         flag "initial-state" no_arg ~doc:"Print the initial state"
       and final_states =
         flag "final-states" no_arg ~doc:"Print the final states"
       and transitions =
         flag "transitions" no_arg ~doc:"Print the transitions"
       in
       fun () ->
         let%bind.Or_error fa =
           In_channel.read_all automaton_file
           |> Sexp.of_string |> Finite_automaton.t_of_sexp
         in
         if alphabet then
           print_endline
             ("Alphabet: " ^ String.of_list (Finite_automaton.get_alphabet fa));
         if states then
           print_endline
             ("States: " ^ String.of_list (Finite_automaton.get_states fa));
         if initial_state then
           print_endline
             ("Initial state: "
             ^ String.of_char (Finite_automaton.get_initial_state fa));
         if final_states then
           print_endline
             ("Final states: "
             ^ String.of_list (Finite_automaton.get_final_states fa));
         if transitions then (
           print_endline "Transitions:";
           Finite_automaton.get_transitions fa
           |> List.map ~f:(fun (x, y, z) -> String.of_list [ x; y; z ])
           |> List.iter ~f:print_endline);
         Ok ())

  let does_accept_command =
    Command.basic_or_error
      ~summary:"Check if a string is accepted by the automaton"
      (let%map_open.Command automaton_file = automaton_param
       and input_string = anon ("input_string" %: string) in
       fun () ->
         let%bind.Or_error fa =
           In_channel.read_all automaton_file
           |> Sexp.of_string |> Finite_automaton.t_of_sexp
         in
         let is_accepted = Finite_automaton.does_accept_exn fa input_string in
         print_endline [%string "The string is accepted: %{is_accepted#Bool}"];
         Ok ())

  let command =
    Command.group ~summary:"Commands for finite automata"
      [ ("describe", describe_command); ("does-accept", does_accept_command) ]
end

module Scanner_command = struct
  let scan =
    let int_constant = "^(0|[+-]?[1-9][0-9]*)" in
    let double_constant = "^(0|[+-]?[1-9][0-9]*(\\.[0-9])?)" in
    let str_constant = "^(\"[^\"]*\")" in
    let constants = [ int_constant; double_constant; str_constant ] in
    let identifiers = [ "^([a-z][a-z0-9_]*)" ] in
    Scanner.scan_with_tokens_data ~constants ~identifiers

  let command =
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
end

let () =
  Command_unix.run
    (Command.group ~summary:"Helper commands"
       [
         ("scan", Scanner_command.command);
         ("finite-automaton", Finite_automaton_command.command);
       ])
