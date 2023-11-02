open! Core

let%expect_test "test symbol table operations" =
  let t = Symbol_table.create () in
  let index_2 = Symbol_table.add_symbol t "2" in
  print_s [%message (index_2 : int)];
  [%expect {| (index_2 50) |}];
  let index_2 = Symbol_table.add_symbol t "2" in
  print_s [%message (index_2 : int)];
  [%expect {| (index_2 50) |}];
  let index_test = Symbol_table.add_symbol t "test" in
  print_s [%message (index_test : int)];
  [%expect {| (index_test 358850) |}];
  let symbol_2 = Symbol_table.get_symbol t index_2 in
  print_s [%message (symbol_2 : string option)];
  [%expect {| (symbol_2 (2)) |}];
  let symbol_test = Symbol_table.get_symbol t index_2 in
  print_s [%message (symbol_test : string option)];
  [%expect {| (symbol_test (2)) |}];
  let symbol_not_found = Symbol_table.get_symbol t 0 in
  print_s [%message (symbol_not_found : string option)];
  [%expect {| (symbol_not_found ()) |}]

let%expect_test "test scanner" =
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
  let identifiers = "([a-z_][a-z0-9_]*)" |> append_operator_or_separator in
  let identifiers = [ Re2.create_exn identifiers ] in
  let scan =
    Scanner.scan ~separators ~operators ~reserved_words ~constants ~identifiers
  in
  let result = scan ~program:"int a;" in
  let st, pif = Or_error.ok_exn result in
  print_s [%message (pif : Pif.t)];
  [%expect
    {|
    ((line "int a;") (token (Reserved_word int)))
    ((line "a;") (token (Identifier a)))
    ((line ";") (token (Separator ";")))
    (pif ((";" -1) (id 97) (int -1))) |}];
  print_s [%message (Symbol_table.get_symbol st 97 : string option)];
  [%expect {| ("Symbol_table.get_symbol st 97" (a)) |}]
