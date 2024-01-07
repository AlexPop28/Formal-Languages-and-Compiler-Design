open! Core

module Finite_automaton_command = struct
  let automaton_param =
    Command.Param.anon (Command.Anons.( %: ) "automaton-file" Filename_unix.arg_type)
  ;;

  let describe_command =
    Command.basic_or_error
      ~summary:"Describe the parsed automaton from a file"
      (let%map_open.Command automaton_file = automaton_param
       and alphabet = flag "alphabet" no_arg ~doc:"Print the alphabet"
       and states = flag "states" no_arg ~doc:"Print all the states"
       and initial_state = flag "initial-state" no_arg ~doc:"Print the initial state"
       and final_states = flag "final-states" no_arg ~doc:"Print the final states"
       and transitions = flag "transitions" no_arg ~doc:"Print the transitions" in
       fun () ->
         let%bind.Or_error fa =
           In_channel.read_all automaton_file
           |> Sexp.of_string
           |> Finite_automaton.t_of_sexp
         in
         if alphabet
         then
           print_endline ("Alphabet: " ^ String.of_list (Finite_automaton.get_alphabet fa));
         if states
         then print_endline ("States: " ^ String.of_list (Finite_automaton.get_states fa));
         if initial_state
         then
           print_endline
             ("Initial state: " ^ String.of_char (Finite_automaton.get_initial_state fa));
         if final_states
         then
           print_endline
             ("Final states: " ^ String.of_list (Finite_automaton.get_final_states fa));
         if transitions
         then (
           print_endline "Transitions:";
           Finite_automaton.get_transitions fa
           |> List.map ~f:(fun (x, y, z) ->
             String.of_char x ^ " " ^ String.of_char y ^ " [" ^ String.of_list z ^ "]")
           |> List.iter ~f:print_endline);
         Ok ())
  ;;

  let does_accept_command =
    Command.basic_or_error
      ~summary:"Check if a string is accepted by the automaton"
      (let%map_open.Command automaton_file = automaton_param
       and input_string = anon ("input_string" %: string) in
       fun () ->
         let%bind.Or_error fa =
           In_channel.read_all automaton_file
           |> Sexp.of_string
           |> Finite_automaton.t_of_sexp
         in
         let is_accepted = Finite_automaton.does_accept_exn fa input_string in
         print_endline [%string "The string is accepted: %{is_accepted#Bool}"];
         Ok ())
  ;;

  let command =
    Command.group
      ~summary:"Commands for finite automata"
      [ "describe", describe_command; "does-accept", does_accept_command ]
  ;;
end

module Scanner_command = struct
  let command =
    Command.basic_or_error
      ~summary:"Run lexical analysis on a source code file"
      (let open Command.Let_syntax in
       let%map_open file = anon ("source-code-file" %: Filename_unix.arg_type)
       and tokens_file =
         flag "tokens" (required Filename_unix.arg_type) ~doc:"FILE Tokens file"
       and st_out =
         flag "st" (required Filename_unix.arg_type) ~doc:"FILE Symbol table output file"
       and pif_out =
         flag
           "pif"
           (required Filename_unix.arg_type)
           ~doc:"FILE Program internal form output file"
       and constants_file =
         flag
           "constants"
           (required Filename_unix.arg_type)
           ~doc:"FILE Constants file describing an automaton"
       and identifiers_file =
         flag
           "identifiers"
           (required Filename_unix.arg_type)
           ~doc:"FILE Identifiers file describing an automaton"
       in
       fun () ->
         let program = In_channel.read_all file in
         let tokens_data =
           In_channel.read_all tokens_file
           |> Sexp.of_string
           |> Scanner.Tokens_data.t_of_sexp
         in
         let%bind.Or_error constants =
           In_channel.read_all constants_file
           |> Sexp.of_string
           |> Finite_automaton.t_of_sexp
         in
         let%bind.Or_error identifiers =
           In_channel.read_all identifiers_file
           |> Sexp.of_string
           |> Finite_automaton.t_of_sexp
         in
         let%map.Or_error st, pif =
           Scanner.scan_with_tokens_data ~constants ~identifiers ~tokens_data ~program
         in
         Out_channel.write_all st_out ~data:(Symbol_table.to_hum st);
         Out_channel.write_all pif_out ~data:(Pif.to_hum pif);
         print_endline "Lexically correct")
  ;;
end

module Grammar_command = struct
  let grammar_param =
    Command.Param.anon (Command.Anons.( %: ) "grammar-file" Filename_unix.arg_type)
  ;;

  let map_productions_to_string productions =
    List.map productions ~f:(fun (lhp, rhp) ->
      let lhp = String.concat lhp ~sep:" " in
      let rhp = String.concat rhp ~sep:" " in
      [%string "%{lhp} -> %{rhp}"])
    |> String.concat ~sep:" ; "
  ;;

  let describe_command =
    Command.basic_or_error
      ~summary:"Describe the grammar from a file"
      (let%map_open.Command grammar_file = grammar_param
       and terminals = flag "terminals" no_arg ~doc:"Print the terminals"
       and non_terminals = flag "non-terminals" no_arg ~doc:"Print the non-terminals"
       and starting_symbol =
         flag "starting-symbol" no_arg ~doc:"Print the starting symbol"
       and productions = flag "productions" no_arg ~doc:"Print the productions"
       and productions_for_non_terminal =
         flag
           "productions-for-nonterminal"
           (optional string)
           ~doc:"Print the productions for a given non-terminal"
       in
       fun () ->
         let%bind.Or_error grammar = Grammar.create_from_file grammar_file in
         if terminals
         then
           print_endline
             ("Terminals: " ^ String.concat ~sep:" " (Grammar.get_terminals grammar));
         if non_terminals
         then
           print_endline
             ("Non-terminals: "
              ^ String.concat ~sep:" " (Grammar.get_non_terminals grammar));
         if starting_symbol
         then print_endline ("Starting symbol: " ^ Grammar.get_starting_symbol grammar);
         if productions
         then
           print_endline
             ("Productions: "
              ^ map_productions_to_string (Grammar.get_productions grammar));
         let%bind.Or_error () =
           match productions_for_non_terminal with
           | Some non_terminal ->
             let%bind.Or_error productions =
               Grammar.get_productions_of grammar [ non_terminal ]
             in
             print_endline
               ("Productions of "
                ^ non_terminal
                ^ ": "
                ^ map_productions_to_string productions);
             Ok ()
           | None -> Ok ()
         in
         Ok ())
  ;;

  let is_context_free_command =
    Command.basic_or_error
      ~summary:"Check if a grammar from a file is context-free"
      (let%map_open.Command grammar_file = grammar_param in
       fun () ->
         let%bind.Or_error grammar = Grammar.create_from_file grammar_file in
         let is_cfg = Grammar.is_context_free grammar in
         print_s [%message (is_cfg : bool)];
         Ok ())
  ;;

  let command =
    Command.group
      ~summary:"Commands for grammars"
      [ "describe", describe_command; "is-cfg", is_context_free_command ]
  ;;
end

module Parser_command = struct
  let grammar_param =
    Command.Param.flag
      "grammar"
      (Command.Param.required Filename_unix.arg_type)
      ~doc:"FILE Input grammar file"
  ;;

  let pif_param =
    Command.Param.flag
      "pif"
      (Command.Param.required Filename_unix.arg_type)
      ~doc:"FILE Input pif file"
  ;;

  let output_param =
    Command.Param.flag
      "output"
      (Command.Param.optional Filename_unix.arg_type)
      ~doc:"FILE Output file (default: stdout)"
  ;;

  let parse_command =
    Command.basic_or_error
      ~summary:"Parse a program using grammar and pif"
      (let open Command.Let_syntax in
       let%map_open grammar_file = grammar_param
       and pif_file = pif_param
       and output_file = output_param in
       fun () ->
         let%bind.Or_error grammar = Grammar.create_from_file grammar_file in
         let%bind.Or_error grammar = Enhanced_grammar.create grammar in
         let pif = In_channel.read_all pif_file |> Pif.from_hum |> Or_error.ok_exn in
         let parser = Parser.create grammar in
         let program = List.map ~f:fst (Pif.to_list pif) in
         let%bind.Or_error parse_result = Parser.parse parser program in
         let output_channel =
           match output_file with
           | Some file -> Out_channel.create file
           | None -> Out_channel.stdout
         in
         Out_channel.output_string
           output_channel
           (Parser.Parser_output.to_string parse_result);
         if Option.is_some output_file then Out_channel.close output_channel;
         Ok ())
  ;;

  let command = Command.group ~summary:"Commands for parsing" [ "parse", parse_command ]
end

let () =
  Command_unix.run
    (Command.group
       ~summary:"Helper commands"
       [ "scan", Scanner_command.command
       ; "finite-automaton", Finite_automaton_command.command
       ; "grammar", Grammar_command.command
       ; "parser", Parser_command.command
       ])
;;
