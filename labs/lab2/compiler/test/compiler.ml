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
  let symbol_test = Symbol_table.get_symbol t index_test in
  print_s [%message (symbol_test : string option)];
  [%expect {| (symbol_test (test)) |}];
  let symbol_not_found = Symbol_table.get_symbol t 0 in
  print_s [%message (symbol_not_found : string option)];
  [%expect {| (symbol_not_found ()) |}];
  print_string (Symbol_table.to_hum t);
  [%expect {|
    50: 2
    358850: test |}]
;;

let create_tokens_data () : Scanner.Tokens_data.t =
  { operators = [ "+"; "-"; "*"; "/"; "%"; "=="; "<="; "<"; ">="; ">"; "="; "!=" ]
  ; separators = [ "{"; "}"; "("; ")"; ";"; " "; "$" ]
  ; reserved_words =
      [ "int"
      ; "str"
      ; "double"
      ; "if"
      ; "else"
      ; "while"
      ; "get"
      ; "set"
      ; "read_int"
      ; "read_str"
      ; "read_double"
      ; "print_int"
      ; "print_str"
      ; "print_double"
      ]
  }
;;

let%expect_test "test parse tokens.in" =
  let tokens_data = create_tokens_data () in
  print_s [%message (tokens_data : Scanner.Tokens_data.t)];
  [%expect
    {|
    (tokens_data
     ((operators (+ - * / % == <= < >= > = !=))
      (separators ({ } "(" ")" ";" " " $))
      (reserved_words
       (int str double if else while get set read_int read_str read_double
        print_int print_str print_double)))) |}];
  let sexp =
    {|
     ((operators (+ - * / % == <= < >= > = !=))
      (separators ({ } "(" ")" ";" " " $))
      (reserved_words
       (int str double if else while get set read_int read_str read_double
        print_int print_str print_double))) |}
  in
  let tokens_data = Scanner.Tokens_data.t_of_sexp (Sexp.of_string sexp) in
  print_s [%message (tokens_data : Scanner.Tokens_data.t)];
  [%expect
    {|
    (tokens_data
     ((operators (+ - * / % == <= < >= > = !=))
      (separators ({ } "(" ")" ";" " " $))
      (reserved_words
       (int str double if else while get set read_int read_str read_double
        print_int print_str print_double)))) |}]
;;

let%expect_test "awful test to see if the regexps are constructed properly; to be \
                 refactored"
  =
  let t = create_tokens_data () in
  let wrap_each_char s =
    let wrapped = ref "" in
    String.iter s ~f:(fun c ->
      if Char.(c = '$')
      then wrapped := String.of_char c
      else wrapped := !wrapped ^ "[" ^ String.of_char c ^ "]");
    !wrapped
  in
  let reduce = List.reduce_exn ~f:(fun acc op -> acc ^ "|" ^ op) in
  let operators = List.map t.operators ~f:wrap_each_char in
  let separators = List.map t.separators ~f:wrap_each_char in
  let reserved_words = "^(" ^ reduce t.reserved_words ^ ")" in
  let operators = reduce operators in
  let separators = reduce separators in
  print_string operators;
  [%expect "[+]|[-]|[*]|[/]|[%]|[=][=]|[<][=]|[<]|[>][=]|[>]|[=]|[!][=]"];
  print_string separators;
  [%expect "[{]|[}]|[(]|[)]|[;]|[ ]|$"];
  print_string reserved_words;
  [%expect
    "^(int|str|double|if|else|while|get|set|read_int|read_str|read_double|print_int|print_str|print_double)"]
;;

let get_constants_fa () =
  Sexplib.Sexp.of_string
    {|
      ((alphabet (. 0 1 2 3 4 5 6 7 8 9 + - " " "\"" a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
      (states (S A B T R X Y Z))
      (initial_state S)
      (final_states (B R X Z))
      (transitions (
        (S A (+ -))
        (S Z (0))
        (S B (1 2 3 4 5 6 7 8 9))
        (A B (1 2 3 4 5 6 7 8 9))
        (B B (1 2 3 4 5 6 7 8 9 0))
        (B X (.))
        (Y X (.))
        (X X (1 2 3 4 5 6 7 8 9 0))
        (A Y (0))


        (S T ("\""))
        (T T (" " a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z 0 1 2 3 4 5 6 7 8 9))
        (T R ("\""))
      )))|}
  |> Finite_automaton.t_of_sexp
  |> Or_error.ok_exn
;;

let get_identifiers_fa () =
  Sexplib.Sexp.of_string
    {|
      ((alphabet (_ a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z 0 1 2 3 4 5 6 7 8 9))
      (states (S A))
      (initial_state S)
      (final_states (A))
      (transitions (
        (S A (a b c d e f g h i j k l m n o p q r s t u v w x y z))
        (A A (a b c d e f g h i j k l m n o p q r s t u v w x y z 0 1 2 3 4 5 6 7 8 9 _))
      )))|}
  |> Finite_automaton.t_of_sexp
  |> Or_error.ok_exn
;;

let scan =
  let tokens_data = create_tokens_data () in
  let constants = get_constants_fa () in
  let identifiers = get_identifiers_fa () in
  Scanner.scan_with_tokens_data ~tokens_data ~constants ~identifiers
;;

let%expect_test "test scanner easy input" =
  let result = scan ~program:"int a;" in
  let st, pif = Or_error.ok_exn result in
  let pif = Pif.to_list pif in
  print_s [%message (pif : (string * int) list)];
  [%expect {|
    (pif ((int -1) (id 97) (";" -1))) |}];
  print_s [%message (Symbol_table.get_symbol st 97 : string option)];
  [%expect {| ("Symbol_table.get_symbol st 97" (a)) |}]
;;

let%expect_test "test scanner p1 lab1" =
  let result =
    scan
      ~program:
        {|
int a;
int b;
int c;
a = read_int();
b = read_int();
c = read_int();
int ans;
ans = a;
if (b > ans) {
  ans = b
}
if (c > ans) {
  ans = c;
}
print_int(ans);
|}
  in
  let st, pif = Or_error.ok_exn result in
  print_string (Pif.to_hum pif);
  [%expect
    {|
    -1: int
    97: id
    -1: ;
    -1: int
    98: id
    -1: ;
    -1: int
    99: id
    -1: ;
    97: id
    -1: =
    -1: read_int
    -1: (
    -1: )
    -1: ;
    98: id
    -1: =
    -1: read_int
    -1: (
    -1: )
    -1: ;
    99: id
    -1: =
    -1: read_int
    -1: (
    -1: )
    -1: ;
    -1: int
    441021: id
    -1: ;
    441021: id
    -1: =
    97: id
    -1: ;
    -1: if
    -1: (
    98: id
    -1: >
    441021: id
    -1: )
    -1: {
    441021: id
    -1: =
    98: id
    -1: }
    -1: if
    -1: (
    99: id
    -1: >
    441021: id
    -1: )
    -1: {
    441021: id
    -1: =
    99: id
    -1: ;
    -1: }
    -1: print_int
    -1: (
    441021: id
    -1: )
    -1: ; |}];
  print_string (Symbol_table.to_hum st);
  [%expect {|
    97: a
    98: b
    99: c
    441021: ans |}]
;;

let%expect_test "test scanner p2 lab1" =
  let result =
    scan
      ~program:
        {|
int n;
n = read_int();
int prime;
prime = 1;
int d;
d = 2;
while (d < n) {
  if (n % d == 0) {
    prime = 0;
  }
  d = d + 1;
}
if (prime == 0) {
  print_str("not prime")
} else {
  print_str("prime")
}
|}
  in
  let st, pif = Or_error.ok_exn result in
  print_string (Pif.to_hum pif);
  [%expect
    {|
    -1: int
    110: id
    -1: ;
    110: id
    -1: =
    -1: read_int
    -1: (
    -1: )
    -1: ;
    -1: int
    196883: id
    -1: ;
    196883: id
    -1: =
    49: const
    -1: ;
    -1: int
    100: id
    -1: ;
    100: id
    -1: =
    50: const
    -1: ;
    -1: while
    -1: (
    100: id
    -1: <
    110: id
    -1: )
    -1: {
    -1: if
    -1: (
    110: id
    -1: %
    100: id
    -1: ==
    48: const
    -1: )
    -1: {
    196883: id
    -1: =
    48: const
    -1: ;
    -1: }
    100: id
    -1: =
    100: id
    -1: +
    49: const
    -1: ;
    -1: }
    -1: if
    -1: (
    196883: id
    -1: ==
    48: const
    -1: )
    -1: {
    -1: print_str
    -1: (
    649405: const
    -1: )
    -1: }
    -1: else
    -1: {
    -1: print_str
    -1: (
    483306: const
    -1: )
    -1: } |}];
  print_string (Symbol_table.to_hum st);
  [%expect
    {|
    48: 0
    49: 1
    50: 2
    100: d
    110: n
    196883: prime
    483306: "prime"
    649405: "not prime" |}]
;;

let%expect_test "test scanner p3 lab 1" =
  let result =
    scan
      ~program:
        {|
int n;
n = read_int();
int i;
i = 0;
int sum;
sum = 0;
while (i < n) {
  int x;
  x = read_int();
  sum = sum + x;
}
print_int(sum);
|}
  in
  let st, pif = Or_error.ok_exn result in
  print_string (Pif.to_hum pif);
  [%expect
    {|
    -1: int
    110: id
    -1: ;
    110: id
    -1: =
    -1: read_int
    -1: (
    -1: )
    -1: ;
    -1: int
    105: id
    -1: ;
    105: id
    -1: =
    48: const
    -1: ;
    -1: int
    299670: id
    -1: ;
    299670: id
    -1: =
    48: const
    -1: ;
    -1: while
    -1: (
    105: id
    -1: <
    110: id
    -1: )
    -1: {
    -1: int
    120: id
    -1: ;
    120: id
    -1: =
    -1: read_int
    -1: (
    -1: )
    -1: ;
    299670: id
    -1: =
    299670: id
    -1: +
    120: id
    -1: ;
    -1: }
    -1: print_int
    -1: (
    299670: id
    -1: )
    -1: ; |}];
  print_string (Symbol_table.to_hum st);
  [%expect {|
    48: 0
    105: i
    110: n
    120: x
    299670: sum |}]
;;

let%expect_test "test scanner p1err lab 1" =
  let result =
    scan
      ~program:
        {|
int _n;
_n = read_int();
int i; # wrong comment
i = 0;
int sum;
sum = 0;
while (i < _n) {
  int x;
  x = read_int();
  sum = sum + x;
}
print_int(sum);

|}
  in
  print_s [%message (result : (Symbol_table.t * Pif.t) Or_error.t)];
  [%expect
    {|
      (result
       (Error
        ("Lexical error at line 2: _n;" "Lexical error at line 3: _n = read_int();"
         "Lexical error at line 4: # wrong comment"
         "Lexical error at line 8: _n) {"))) |}]
;;

let%expect_test "test finite automaton" =
  let sexp =
    Sexplib.Sexp.of_string
      {|
      ((alphabet (a b c))
      (states (A B C))
      (initial_state A)
      (final_states (C))
      (transitions ((A B (a)) (B B (b)) (B C (c)))))|}
  in
  let fa = Finite_automaton.t_of_sexp sexp |> Or_error.ok_exn in
  let accepts = Finite_automaton.does_accept_exn fa in
  print_s
    [%message
      (Finite_automaton.get_alphabet fa : char list)
        (Finite_automaton.get_initial_state fa : char)
        (Finite_automaton.get_final_states fa : char list)
        (Finite_automaton.get_states fa : char list)
        (Finite_automaton.get_transitions fa : (char * char * char list) list)];
  [%expect
    {|
    (("Finite_automaton.get_alphabet fa" (a b c))
     ("Finite_automaton.get_initial_state fa" A)
     ("Finite_automaton.get_final_states fa" (C))
     ("Finite_automaton.get_states fa" (A B C))
     ("Finite_automaton.get_transitions fa" ((A B (a)) (B B (b)) (B C (c)))))|}];
  print_s [%message (accepts "a" : bool)];
  [%expect {| ("accepts \"a\"" false) |}];
  print_s [%message (accepts "ab" : bool)];
  [%expect {| ("accepts \"ab\"" false) |}];
  print_s [%message (accepts "ac" : bool)];
  [%expect {| ("accepts \"ac\"" true) |}];
  print_s [%message (accepts "abc" : bool)];
  [%expect {| ("accepts \"abc\"" true) |}];
  print_s [%message (accepts "abbc" : bool)];
  [%expect {| ("accepts \"abbc\"" true) |}];
  print_s [%message (accepts "abbbc" : bool)];
  [%expect {| ("accepts \"abbbc\"" true) |}];
  print_s [%message (accepts "abbbbc" : bool)];
  [%expect {| ("accepts \"abbbbc\"" true) |}]
;;

let get_parser () =
  (* TODO: refactor tests to provide a sexp representation for the grammars and
     write a wrapper [with_grammar] that takes it and the test itself as a
     callback *)
  let grammar =
    { non_terminals = [ "S"; "A" ]
    ; terminals = [ "a"; "b"; "c" ]
    ; starting_symbol = "S"
    ; productions = [ [ "S" ], [ "a"; "A" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
    } |> Grammar.validate |> Or_error.ok_exn
  in
  let grammar = Enhanced_grammar.create grammar |> Or_error.ok_exn in
  Parser.create grammar
;;

let%expect_test "TODO [Grammar.create]" = ()

let%expect_test "test closure on empty set" =
  let parser = get_parser () in
  let items = Hash_set.of_list (module Parser.Lr0_item) [] in
  let closure = Parser.For_testing.closure parser items in
  print_string (Parser.State.to_string_hum closure);
  [%expect {| |}]
;;

let%expect_test "test closure basic" =
  let parser = get_parser () in
  let items =
    Hash_set.of_list
      (module Parser.Lr0_item)
      [ { lhp = "S"; left_dot = [ "a" ]; right_dot = [ "A" ] } ]
  in
  let closure = Parser.For_testing.closure parser items in
  print_string (Parser.State.to_string_hum closure);
  [%expect "[S -> a.A] ; [A -> .c] ; [A -> .b A] "]
;;

let%expect_test "test goto to empty state" =
  let parser = get_parser () in
  let items =
    Hash_set.of_list
      (module Parser.Lr0_item)
      [ { lhp = "S"; left_dot = [ "a" ]; right_dot = [ "A" ] } ]
  in
  let state : Parser.State.t = { items } in
  let result = Parser.For_testing.goto parser state "B" |> Or_error.ok_exn in
  print_string (Parser.State.to_string_hum result);
  [%expect ""]
;;

let%expect_test "test goto" =
  let parser = get_parser () in
  let items =
    Hash_set.of_list
      (module Parser.Lr0_item)
      [ { lhp = "S"; left_dot = []; right_dot = [ "a"; "A" ] } ]
  in
  let state : Parser.State.t = { items } in
  let result = Parser.For_testing.goto parser state "a" |> Or_error.ok_exn in
  print_string (Parser.State.to_string_hum result);
  [%expect "[S -> a.A] ; [A -> .c] ; [A -> .b A]"]
;;

let%expect_test "test canonical collection basic" =
  let parser = get_parser () in
  let parsing_table = Parser.get_parsing_table parser |> Or_error.ok_exn in
  let canonical_collection_string =
    Parser.Parsing_table.For_testing.get_canonical_collection_string parsing_table
  in
  print_string canonical_collection_string;
  [%expect
    {|
      6: [S' -> S.]
      5: [S -> a A.]
      4: [A -> b A.]
      3: [A -> c.]
      2: [A -> .c] ; [A -> b.A] ; [A -> .b A]
      1: [S -> a.A] ; [A -> .c] ; [A -> .b A]
      0: [S' -> .S] ; [S -> .a A] |}]
;;

let%expect_test "test validate fails for undeclared symbol root" =
  let grammar =
    Grammar.create
      ~non_terminals:[ "S"; "A" ]
      ~terminals:[ "a"; "b"; "c" ]
      ~starting_symbol:"S2" (* TODO: validate productions in the create *)
      ~productions:[ [ "S" ], [ "a"; "A"; "S" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
  in
  print_s [%sexp (grammar : Grammar.t Or_error.t)];
  [%expect {|(Error ((symbol S2) " is undeclared"))|}]
;;

let%expect_test "test validate fails for undeclared symbol on lhs" =
  let grammar =
    Grammar.create
      ~non_terminals:[ "S"; "A" ]
      ~terminals:[ "b"; "c" ]
      ~starting_symbol:"S" (* TODO: validate productions in the create *)
      ~productions:[ [ "S" ], [ "a"; "A" ]; [ "A2" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
  in
  print_s [%sexp (grammar : Grammar.t Or_error.t)];
  [%expect
    {|
    (Error
     (((symbol a) " is undeclared") ((symbol A2) " is undeclared")
      (("(lhs, rhs)" ((A2) (b A))) " does not have nonterminal on the left")))|}]
;;

let%expect_test "test validate fails for undeclared symbol on rhs" =
  let grammar =
    Grammar.create
      ~non_terminals:[ "S"; "A" ]
      ~terminals:[ "b"; "c" ]
      ~starting_symbol:"S" (* TODO: validate productions in the create *)
      ~productions:[ [ "S" ], [ "a2"; "1A" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
  in
  print_s [%sexp (grammar : Grammar.t Or_error.t)];
  [%expect
    {|
     (Error (((symbol a2) " is undeclared") ((symbol 1A) " is undeclared")))|}]
;;

let%expect_test "test validate fails if production doesnt have nonterminal on lhs" =
  let grammar =
    Grammar.create
      ~non_terminals:[ "S"; "A" ]
      ~terminals:[ "a"; "b"; "c" ]
      ~starting_symbol:"S" (* TODO: validate productions in the create *)
      ~productions:
        [ [ "S" ], [ "a"; "A" ]
        ; [ "A" ], [ "b"; "A" ]
        ; [ "A" ], [ "c" ]
        ; [ "a" ], [ "a" ]
        ]
  in
  print_s [%sexp (grammar : Grammar.t Or_error.t)];
  [%expect
    {|
    (Error (("(lhs, rhs)" ((a) (a))) " does not have nonterminal on the left"))|}]
;;

let%expect_test "test validate fails if nonterminal doesn't have expansion" =
  let grammar =
    Grammar.create
      ~non_terminals:[ "S"; "A"; "B" ]
      ~terminals:[ "a"; "b"; "c" ]
      ~starting_symbol:"S" (* TODO: validate productions in the create *)
      ~productions:[ [ "S" ], [ "a"; "A"; "B" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
  in
  print_s [%sexp (grammar : Grammar.t Or_error.t)];
  [%expect {|
    (Error ((nonterminal B) " does not have any production"))|}]
;;

let%expect_test "test validate fails if symbol cannot be obtained" =
  let grammar =
    Grammar.create
      ~non_terminals:[ "S"; "A"; "B" ]
      ~terminals:[ "a"; "b"; "c"; "d" ]
      ~starting_symbol:"S" (* TODO: validate productions in the create *)
      ~productions:[ [ "S" ], [ "a"; "A" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
  in
  print_s [%sexp (grammar : Grammar.t Or_error.t)];
  [%expect
    {|
    (Error
     (((nonterminal B) " does not have any production")
      ((symbol d) " cannot be obtained") ((symbol B) " cannot be obtained")))|}]
;;

let get_language_grammar =
  let grammar =
    Sexplib.Sexp.of_string
      {|

      (
        (non_terminals
          (
            identifier
            identifier_after_start
            letter
            digit
            int_constant
            int_after_start
            positive_int
            non_zero_digit
            sign
            str_constant_inside
            str_constant
            double_constant

            program
            statement
            variable_declaration
            function_call
            assignment
            if_expression
            while_expression
            type
            get_call
            set_call
            read_call
            print_call
            expression
            int_expression
            bool_expression
            double_expression
            str_expression
            constant
            bool_operator
            int_term
            int_factor
            double_term
            double_factor
            str_term
          )
        )
        (terminals
          (a b c d e f g h i j k l m n o p q r s t u v w x y z 0 1 2 3 4 5 6 7 8 9 "_" "+" "-" "*" "/" "%" "==" "<" "<=" ">" ">=" "=" "!=" "."
           "{" "}" "[" "]" "," "(" ")" ";" " " "\"" int str double get set read_int read_str read_double print_int print_str print_double if else while)
        )
        (starting_symbol program)
        (productions
          (
            ((letter) (a))
            ((letter) (b))
            ((letter) (c))
            ((letter) (d))
            ((letter) (e))
            ((letter) (f))
            ((letter) (g))
            ((letter) (h))
            ((letter) (i))
            ((letter) (j))
            ((letter) (k))
            ((letter) (l))
            ((letter) (m))
            ((letter) (n))
            ((letter) (o))
            ((letter) (p))
            ((letter) (q))
            ((letter) (r))
            ((letter) (s))
            ((letter) (t))
            ((letter) (u))
            ((letter) (v))
            ((letter) (w))
            ((letter) (x))
            ((letter) (y))
            ((letter) (z))

            ((non_zero_digit) (1))
            ((non_zero_digit) (2))
            ((non_zero_digit) (3))
            ((non_zero_digit) (4))
            ((non_zero_digit) (5))
            ((non_zero_digit) (6))
            ((non_zero_digit) (7))
            ((non_zero_digit) (8))
            ((non_zero_digit) (9))

            ((digit) (0))
            ((digit) (non_zero_digit))

            ((sign) (+))
            ((sign) (-))

            ((identifier_after_start) (letter))
            ((identifier_after_start) (digit))
            ((identifier_after_start) (_))
            ((identifier_after_start) (letter identifier_after_start))
            ((identifier_after_start) (digit identifier_after_start))
            ((identifier_after_start) (_ identifier_after_start))

            ((identifier) (letter))
            ((identifier) (_))
            ((identifier) (letter identifier_after_start))
            ((identifier) (_ identifier_after_start))

            ((int_after_start) (digit))
            ((int_after_start) (digit int_after_start))

            ((positive_int) (non_zero_digit int_after_start))

            ((int_constant) (0))
            ((int_constant) (positive_int))
            ((int_constant) (sign positive_int))

            ((str_constant_inside) (letter))
            ((str_constant_inside) (digit))
            ((str_constant_inside) ("_"))
            ((str_constant_inside) (" "))
            ((str_constant_inside) (letter str_constant_inside))
            ((str_constant_inside) (digit str_constant_inside))
            ((str_constant_inside) ("_" str_constant_inside))
            ((str_constant_inside) (" " str_constant_inside))

            ((str_constant) ("\"" str_constant_inside "\""))

            ((double_constant) (int_constant . int_after_start))
            ((double_constant) (sign 0 . int_after_start))



            ((program) (statement))
            ((program) (program statement))
            ((statement) (variable_declaration ";"))
            ((statement) (function_call ";"))
            ((statement) (assignment ";"))
            ((statement) (if_expression))
            ((statement) (while_expression))

            ((type) (int))
            ((type) (str))
            ((type) (double))

            ((variable_declaration) (type identifier))
            ((variable_declaration) (type "[" identifier "]"))

            ((function_call) (get_call))
            ((function_call) (set_call))
            ((function_call) (read_call))
            ((function_call) (print_call))

            ((get_call) (get "(" identifier "," int_expression ")"))

            ((set_call) (set "(" identifier "," int_expression "," expression ")"))

            ((read_call) (read_int "(" ")"))
            ((read_call) (read_str "(" ")"))
            ((read_call) (read_double "(" ")"))

            ((print_call) (print_int "(" expression ")"))
            ((print_call) (print_str "(" expression ")"))
            ((print_call) (print_double "(" expression ")"))

            ((assignment) (identifier "=" expression))

            ((if_expression) (if "(" bool_expression ")" "{" program "}"))
            ((if_expression) (if "(" bool_expression ")" "{" program "}" else "{" program "}"))

            ((while_expression) (while "(" bool_expression ")" "{" program "}"))

            ((constant) (int_constant))
            ((constant) (str_constant))
            ((constant) (double_constant))

            ((bool_operator) ("=="))
            ((bool_operator) ("!="))
            ((bool_operator) ("<"))
            ((bool_operator) (">"))
            ((bool_operator) ("<="))
            ((bool_operator) (">="))

            ((bool_expression) (constant bool_operator constant))
            ((bool_expression) (constant bool_operator identifier))
            ((bool_expression) (identifier bool_operator constant))
            ((bool_expression) (identifier bool_operator identifier))

            ((expression) (int_expression))
            ((expression) (double_expression))
            ((expression) (str_expression))

            ((int_expression) (int_term))
            ((int_expression) (int_term "+" int_expression))
            ((int_expression) (int_term "-" int_expression))

            ((int_term) (int_factor))
            ((int_term) (int_factor "*" int_term))
            ((int_term) (int_factor "/" int_term))
            ((int_term) (int_factor "%" int_term))

            ((int_factor) (int_constant))
            ((int_factor) (identifier))
            ((int_factor) ("(" int_expression ")"))

            ((double_expression) (double_term))
            ((double_expression) (double_term "+" double_expression))
            ((double_expression) (double_term "-" double_expression))

            ((double_term) (double_factor))
            ((double_term) (double_factor "*" int_term))
            ((double_term) (double_factor "/" int_term))

            ((double_factor) (double_constant))
            ((double_factor) (identifier))
            ((double_factor) ("(" double_expression ")"))

            ((str_expression) (str_term))
            ((str_expression) (str_term "+" str_expression))

            ((str_term) (str_expression))
            ((str_term) (identifier))
          )
        )
      )
    |}
    |> Grammar.t_of_sexp
    |> Grammar.validate
  in
  grammar
;;

let%expect_test "test gramatic" =
  let grammar = get_language_grammar |> Or_error.map ~f:(fun _ -> ()) in
  print_s [%sexp (grammar : unit Or_error.t)];
  [%expect "(Ok ())"]
;;

let%expect_test "test get production fails if lhs contains undeclared symbols" =
  let grammar =
    Grammar.create
      ~non_terminals:[ "S"; "A" ]
      ~terminals:[ "a"; "b"; "c" ]
      ~starting_symbol:"S" (* TODO: validate productions in the create *)
      ~productions:[ [ "S" ], [ "a"; "A" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
    |> Or_error.ok_exn
  in
  let productions = Grammar.get_productions_of grammar [ "d" ] in
  print_s [%sexp (productions : _ Or_error.t)];
  [%expect {|
    (Error ((symbol d) " is undeclared"))|}]
;;

let%expect_test "test get production is ok" =
  let grammar =
    Grammar.create
      ~non_terminals:[ "S"; "A" ]
      ~terminals:[ "a"; "b"; "c" ]
      ~starting_symbol:"S" (* TODO: validate productions in the create *)
      ~productions:[ [ "S" ], [ "a"; "A" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
    |> Or_error.ok_exn
  in
  let productions = Grammar.get_productions_of grammar [ "A" ] in
  print_s [%sexp (productions : (string list * string list) list Or_error.t)];
  [%expect {|
    (Ok (((A) (b A)) ((A) (c))))|}]
;;

(* TODO Uncomment this to check conflicts in our grammar *)
(*
let%expect_test "test canonical collection our grammar" =
  let grammar = get_language_grammar |> ok_exn |> Enhanced_grammar.create |> ok_exn in
  let parser = Parser.create grammar in
  let parsing_table = Parser.get_parsing_table parser |> ok_exn in
  let canonical_collection =
    Parser.Parsing_table.For_testing.get_canonical_collection parsing_table
  in
  List.iter canonical_collection ~f:(fun (state, _id) ->
    let action = Parser.State.get_action state grammar in
    print_s [%sexp (action : Parser.State.Action.t Or_error.t)]);
  [%expect {||}]
;;
*)
(*TODO test it fails on invalid output bands*)
let%expect_test "test parser output works toy grammar" =
  let grammar =
    { non_terminals = [ "S"; "A" ]
    ; terminals = [ "a"; "b"; "c" ]
    ; starting_symbol = "S" (* TODO: validate productions in the create *)
    ; productions = [ [ "S" ], [ "a"; "A" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
    } |> Grammar.validate
  in
  match grammar with 
  | Error _ -> print_s [%sexp (grammar: Grammar.t Or_error.t)]
  | Ok grammar -> (
    (*
       abbbbc
       1 2 2 2 2 3
    *)
    let grammar = Enhanced_grammar.create grammar |> Or_error.ok_exn in
    let parser = Parser.create grammar in
    let parser_output = Parser.parse parser [ "a"; "b"; "b"; "b"; "b"; "c" ] |> ok_exn in
    print_string (Parser.Parser_output.to_string parser_output);
  );
  [%expect
    {|
    |   0. |          S |   1 |   - |
    |   1. |          a |   - |   2 |
    |   2. |          A |   3 |   - |
    |   3. |          b |   - |   4 |
    |   4. |          A |   5 |   - |
    |   5. |          b |   - |   6 |
    |   6. |          A |   7 |   - |
    |   7. |          b |   - |   8 |
    |   8. |          A |   9 |   - |
    |   9. |          b |   - |  10 |
    |  10. |          A |  11 |   - |
    |  11. |          c |   - |   - | |}]
;;

let%expect_test "test parser works on toy grammar" =
  let grammar =
    { non_terminals = [ "S"; "A" ]
    ; terminals = [ "a"; "b"; "c" ]
    ; starting_symbol = "S"
    ; productions = [ [ "S" ], [ "a"; "A" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
    } |> Grammar.validate
  in
  match grammar with 
  | Error _ -> print_s [%sexp (grammar: Grammar.t Or_error.t)]
  | Ok grammar -> (
    (*
       abbbbc
       1 2 2 2 2 3
    *)
    let grammar = Enhanced_grammar.create grammar |> Or_error.ok_exn in
    let parser = Parser.create grammar in
    let parser_output = Parser.parse parser [ "a"; "b"; "b"; "b"; "b"; "c" ] |> ok_exn in
    print_string (Parser.Parser_output.to_string parser_output);
  );
  [%expect
    {|
    |   0. |          S |   1 |   - |
    |   1. |          a |   - |   2 |
    |   2. |          A |   3 |   - |
    |   3. |          b |   - |   4 |
    |   4. |          A |   5 |   - |
    |   5. |          b |   - |   6 |
    |   6. |          A |   7 |   - |
    |   7. |          b |   - |   8 |
    |   8. |          A |   9 |   - |
    |   9. |          b |   - |  10 |
    |  10. |          A |  11 |   - |
    |  11. |          c |   - |   - | |}]
;;

let%expect_test "test parser on wrong input grammar" =
  let grammar =
    { non_terminals = [ "S"; "A" ]
    ; terminals = [ "a"; "b"; "c" ]
    ; starting_symbol = "S" 
    ; productions = [ [ "S" ], [ "a"; "A" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
    } |> Grammar.validate
  in
  match grammar with 
  | Error _ -> print_s [%sexp (grammar: Grammar.t Or_error.t)]
  | Ok grammar -> (
    let grammar = Enhanced_grammar.create grammar |> Or_error.ok_exn in
    let parser = Parser.create grammar in
    let parser_output = Parser.parse parser [ "a"; "b"; "b"; "d"; "b"; "c" ] in
    print_s [%sexp (parser_output : Parser.Parser_output.t Or_error.t)]
  );
  [%expect {|
    (Error ("Goto to empty state" (state 2) d)) |}]
;;

let%expect_test "test parser works on more serious grammar" =
  let grammar : Grammar.t =
    { non_terminals = [ "S" ]
    ; terminals = [ "a"; "b"; "c"; "d" ]
    ; starting_symbol = "S"
    ; productions =
        [ [ "S" ], [ "a"; "S"; "b" ]; [ "S" ], [ "a"; "S"; "c" ]; [ "S" ], [ "d"; "b" ] ]
    }
  in
  let grammar = Grammar.validate grammar in 
  match grammar with 
  | Error _ -> print_s[%sexp (grammar: Grammar.t Or_error.t)]
  | Ok grammar -> (
    let grammar = Enhanced_grammar.create grammar |> Or_error.ok_exn in
    let parser = Parser.create grammar in
    let parser_output =
      Parser.parse parser [ "a"; "a"; "a"; "d"; "b"; "c"; "b"; "b" ] |> ok_exn
    in
    print_string (Parser.Parser_output.to_string parser_output);
  );
  [%expect
    {|
    |   0. |          S |   1 |   - |
    |   1. |          a |   - |   2 |
    |   2. |          S |   4 |   3 |
    |   3. |          b |   - |   - |
    |   4. |          a |   - |   5 |
    |   5. |          S |   7 |   6 |
    |   6. |          b |   - |   - |
    |   7. |          a |   - |   8 |
    |   8. |          S |  10 |   9 |
    |   9. |          c |   - |   - |
    |  10. |          d |   - |  11 |
    |  11. |          b |   - |   - | |}]
;;
