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
    }
    |> Grammar.validate
    |> Or_error.ok_exn
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
  let result = Parser.For_testing.goto parser state "b" |> Or_error.ok_exn in
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

let%expect_test "test goto fails if symbol not part of grammar" =
  let parser = get_parser () in
  let items =
    Hash_set.of_list
      (module Parser.Lr0_item)
      [ { lhp = "S"; left_dot = [ "a" ]; right_dot = [ "A" ] } ]
  in
  let state : Parser.State.t = { items } in
  let result = Parser.For_testing.goto parser state "B" in
  print_s [%sexp (result : Parser.State.t Or_error.t)];
  [%expect "(Error \"B is not part of grammar\")"]
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
      ~starting_symbol:"S2"
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
      ~starting_symbol:"S"
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
      ~starting_symbol:"S"
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
      ~starting_symbol:"S"
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
      ~starting_symbol:"S"
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
      ~starting_symbol:"S"
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
            start
            program
            statement
            variable_declaration
            assignment
            if_expression
            while_expression
            type
            get_call
            set_call
            read_call
            print_call
            
            bool_operator 

            expression
            term
          )
        )
        (terminals
          ("+" "-" "*" "/" "%" "==" "<" "<=" ">" ">=" "=" "!=" "{" "}" "(" ")" ";" "," int str double get set read_int read_str read_double print_int print_str print_double if else while id const)
        )
        (starting_symbol start)
        (productions
          (
            ((start) ("{" program "}"))
            ((program) (statement))
            ((program) (program statement))
            ((statement) (variable_declaration ";"))
            ((statement) (expression ";"))
            ((statement) (assignment))
            ((statement) (if_expression))
            ((statement) (while_expression))

            ((type) (int))
            ((type) (str))
            ((type) (double))

            ((variable_declaration) (type id))
  
            ((expression) (get_call))
            ((expression) (set_call))
            ((expression) (read_call))
            ((expression) (print_call))

            ((get_call) (get "(" expression "," expression ")"))
            ((set_call) (set "(" expression "," expression "," expression ")"))

            ((read_call) (read_int "(" ")"))
            ((read_call) (read_str "(" ")"))
            ((read_call) (read_double "(" ")"))

            ((print_call) (print_int "(" expression ")"))
            ((print_call) (print_str "(" expression ")"))
            ((print_call) (print_double "(" expression ")"))

            ((assignment) (expression "=" expression ";"))

            ((if_expression) (if "(" expression bool_operator expression ")" "{" program "}" ";"))
            ((if_expression) (if "(" expression bool_operator expression ")" "{" program "}" else "{" program "}" ";"))

            ((while_expression) (while "(" expression bool_operator expression ")" "{" program "}"))

            ((bool_operator) ("=="))
            ((bool_operator) ("!="))
            ((bool_operator) ("<"))
            ((bool_operator) (">"))
            ((bool_operator) ("<="))
            ((bool_operator) (">="))

            ((expression) (term))
            ((expression) (expression "+" term))
            ((expression) (expression "-" term))
            ((expression) (expression "*" term))
            ((expression) (expression "/" term))
            ((expression) (expression "%" term))

            ((term) (id))
            ((term) (const))
            ((term) ("(" expression ")"))
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
      ~starting_symbol:"S"
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
      ~starting_symbol:"S"
      ~productions:[ [ "S" ], [ "a"; "A" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
    |> Or_error.ok_exn
  in
  let productions = Grammar.get_productions_of grammar [ "A" ] in
  print_s [%sexp (productions : (string list * string list) list Or_error.t)];
  [%expect {|
    (Ok (((A) (b A)) ((A) (c))))|}]
;;

(* TODO Uncomment this to check conflicts in our grammar *)
let%expect_test "test canonical collection our grammar" =
  let grammar = get_language_grammar |> ok_exn |> Enhanced_grammar.create |> ok_exn in
  let parser = Parser.create grammar in
  let parsing_table = Parser.get_parsing_table parser |> ok_exn in
  let canonical_collection =
    Parser.Parsing_table.For_testing.get_canonical_collection parsing_table
  in
  (*
     List.iter canonical_collection ~f:(fun (state, _id) ->
     let action = Parser.State.get_action state grammar in
     print_s [%sexp (action : Parser.State.Action.t Or_error.t)]);
  *)
  print_s [%sexp (canonical_collection : (Parser.State.t * int) list)];
  [%expect
    {|
    (((((lhp S') (left_dot (start)) (right_dot ()))) 106)
     ((((lhp start) (left_dot (} program {)) (right_dot ()))) 105)
     ((((lhp type) (left_dot ()) (right_dot (int)))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } else {
          program } ";")))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp type) (left_dot ()) (right_dot (str)))
       ((lhp statement) (left_dot ()) (right_dot (expression ";")))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp statement) (left_dot ()) (right_dot (while_expression)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp expression) (left_dot ()) (right_dot (set_call)))
       ((lhp assignment) (left_dot ()) (right_dot (expression = expression ";")))
       ((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp statement) (left_dot ()) (right_dot (variable_declaration ";")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp program) (left_dot (program)) (right_dot (statement)))
       ((lhp while_expression) (left_dot ())
        (right_dot
         (while "(" expression bool_operator expression ")" { program })))
       ((lhp variable_declaration) (left_dot ()) (right_dot (type id)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp statement) (left_dot ()) (right_dot (assignment)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp type) (left_dot ()) (right_dot (double)))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp start) (left_dot (program {)) (right_dot (})))
       ((lhp statement) (left_dot ()) (right_dot (if_expression)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } ";"))))
      104)
     ((((lhp if_expression)
        (left_dot
         (";" } program { else } program { ")" expression bool_operator
          expression "(" if))
        (right_dot ())))
      103)
     ((((lhp if_expression)
        (left_dot
         (} program { else } program { ")" expression bool_operator expression
          "(" if))
        (right_dot (";"))))
      102)
     ((((lhp type) (left_dot ()) (right_dot (int)))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } else {
          program } ";")))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp type) (left_dot ()) (right_dot (str)))
       ((lhp statement) (left_dot ()) (right_dot (expression ";")))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp statement) (left_dot ()) (right_dot (while_expression)))
       ((lhp if_expression)
        (left_dot
         (program { else } program { ")" expression bool_operator expression "("
          if))
        (right_dot (} ";")))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp expression) (left_dot ()) (right_dot (set_call)))
       ((lhp assignment) (left_dot ()) (right_dot (expression = expression ";")))
       ((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp statement) (left_dot ()) (right_dot (variable_declaration ";")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp program) (left_dot (program)) (right_dot (statement)))
       ((lhp while_expression) (left_dot ())
        (right_dot
         (while "(" expression bool_operator expression ")" { program })))
       ((lhp variable_declaration) (left_dot ()) (right_dot (type id)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp statement) (left_dot ()) (right_dot (assignment)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp type) (left_dot ()) (right_dot (double)))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp statement) (left_dot ()) (right_dot (if_expression)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } ";"))))
      101)
     ((((lhp type) (left_dot ()) (right_dot (int)))
       ((lhp program) (left_dot ()) (right_dot (program statement)))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } else {
          program } ";")))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp type) (left_dot ()) (right_dot (str)))
       ((lhp statement) (left_dot ()) (right_dot (expression ";")))
       ((lhp program) (left_dot ()) (right_dot (statement)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp statement) (left_dot ()) (right_dot (while_expression)))
       ((lhp if_expression)
        (left_dot
         ({ else } program { ")" expression bool_operator expression "(" if))
        (right_dot (program } ";")))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp expression) (left_dot ()) (right_dot (set_call)))
       ((lhp assignment) (left_dot ()) (right_dot (expression = expression ";")))
       ((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp statement) (left_dot ()) (right_dot (variable_declaration ";")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp while_expression) (left_dot ())
        (right_dot
         (while "(" expression bool_operator expression ")" { program })))
       ((lhp variable_declaration) (left_dot ()) (right_dot (type id)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp statement) (left_dot ()) (right_dot (assignment)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp type) (left_dot ()) (right_dot (double)))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp statement) (left_dot ()) (right_dot (if_expression)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } ";"))))
      100)
     ((((lhp if_expression)
        (left_dot
         (else } program { ")" expression bool_operator expression "(" if))
        (right_dot ({ program } ";"))))
      99)
     ((((lhp if_expression)
        (left_dot
         (";" } program { ")" expression bool_operator expression "(" if))
        (right_dot ())))
      98)
     ((((lhp if_expression)
        (left_dot (} program { ")" expression bool_operator expression "(" if))
        (right_dot (";")))
       ((lhp if_expression)
        (left_dot (} program { ")" expression bool_operator expression "(" if))
        (right_dot (else { program } ";"))))
      97)
     ((((lhp type) (left_dot ()) (right_dot (int)))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } else {
          program } ";")))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp type) (left_dot ()) (right_dot (str)))
       ((lhp statement) (left_dot ()) (right_dot (expression ";")))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp statement) (left_dot ()) (right_dot (while_expression)))
       ((lhp if_expression)
        (left_dot (program { ")" expression bool_operator expression "(" if))
        (right_dot (} ";")))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp expression) (left_dot ()) (right_dot (set_call)))
       ((lhp assignment) (left_dot ()) (right_dot (expression = expression ";")))
       ((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp statement) (left_dot ()) (right_dot (variable_declaration ";")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp if_expression)
        (left_dot (program { ")" expression bool_operator expression "(" if))
        (right_dot (} else { program } ";")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp program) (left_dot (program)) (right_dot (statement)))
       ((lhp while_expression) (left_dot ())
        (right_dot
         (while "(" expression bool_operator expression ")" { program })))
       ((lhp variable_declaration) (left_dot ()) (right_dot (type id)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp statement) (left_dot ()) (right_dot (assignment)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp type) (left_dot ()) (right_dot (double)))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp statement) (left_dot ()) (right_dot (if_expression)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } ";"))))
      96)
     ((((lhp program) (left_dot (statement)) (right_dot ()))) 95)
     ((((lhp statement) (left_dot (";" expression)) (right_dot ()))) 94)
     ((((lhp assignment) (left_dot (";" expression = expression)) (right_dot ())))
      93)
     ((((lhp assignment) (left_dot (expression = expression)) (right_dot (";")))
       ((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term))))
      92)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp assignment) (left_dot (= expression)) (right_dot (expression ";")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      91)
     ((((lhp assignment) (left_dot (expression)) (right_dot (= expression ";")))
       ((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp statement) (left_dot (expression)) (right_dot (";")))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term))))
      90)
     ((((lhp variable_declaration) (left_dot (id type)) (right_dot ()))) 89)
     ((((lhp variable_declaration) (left_dot (type)) (right_dot (id)))) 88)
     ((((lhp statement) (left_dot (while_expression)) (right_dot ()))) 87)
     ((((lhp statement) (left_dot (if_expression)) (right_dot ()))) 86)
     ((((lhp statement) (left_dot (assignment)) (right_dot ()))) 85)
     ((((lhp statement) (left_dot (";" variable_declaration)) (right_dot ())))
      84)
     ((((lhp statement) (left_dot (variable_declaration)) (right_dot (";")))) 83)
     ((((lhp program) (left_dot (statement program)) (right_dot ()))) 82)
     ((((lhp while_expression)
        (left_dot
         (} program { ")" expression bool_operator expression "(" while))
        (right_dot ())))
      81)
     ((((lhp type) (left_dot ()) (right_dot (int)))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } else {
          program } ";")))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp type) (left_dot ()) (right_dot (str)))
       ((lhp while_expression)
        (left_dot (program { ")" expression bool_operator expression "(" while))
        (right_dot (})))
       ((lhp statement) (left_dot ()) (right_dot (expression ";")))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp statement) (left_dot ()) (right_dot (while_expression)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp expression) (left_dot ()) (right_dot (set_call)))
       ((lhp assignment) (left_dot ()) (right_dot (expression = expression ";")))
       ((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp statement) (left_dot ()) (right_dot (variable_declaration ";")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp program) (left_dot (program)) (right_dot (statement)))
       ((lhp while_expression) (left_dot ())
        (right_dot
         (while "(" expression bool_operator expression ")" { program })))
       ((lhp variable_declaration) (left_dot ()) (right_dot (type id)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp statement) (left_dot ()) (right_dot (assignment)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp type) (left_dot ()) (right_dot (double)))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp statement) (left_dot ()) (right_dot (if_expression)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } ";"))))
      80)
     ((((lhp type) (left_dot ()) (right_dot (int)))
       ((lhp program) (left_dot ()) (right_dot (program statement)))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } else {
          program } ";")))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp type) (left_dot ()) (right_dot (str)))
       ((lhp statement) (left_dot ()) (right_dot (expression ";")))
       ((lhp program) (left_dot ()) (right_dot (statement)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp statement) (left_dot ()) (right_dot (while_expression)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp expression) (left_dot ()) (right_dot (set_call)))
       ((lhp assignment) (left_dot ()) (right_dot (expression = expression ";")))
       ((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp statement) (left_dot ()) (right_dot (variable_declaration ";")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp while_expression)
        (left_dot ({ ")" expression bool_operator expression "(" while))
        (right_dot (program })))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp while_expression) (left_dot ())
        (right_dot
         (while "(" expression bool_operator expression ")" { program })))
       ((lhp variable_declaration) (left_dot ()) (right_dot (type id)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp statement) (left_dot ()) (right_dot (assignment)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp type) (left_dot ()) (right_dot (double)))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp statement) (left_dot ()) (right_dot (if_expression)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } ";"))))
      79)
     ((((lhp while_expression)
        (left_dot (")" expression bool_operator expression "(" while))
        (right_dot ({ program }))))
      78)
     ((((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term)))
       ((lhp while_expression)
        (left_dot (expression bool_operator expression "(" while))
        (right_dot (")" { program }))))
      77)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp while_expression) (left_dot (bool_operator expression "(" while))
        (right_dot (expression ")" { program })))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      76)
     ((((lhp bool_operator) (left_dot ()) (right_dot (>)))
       ((lhp bool_operator) (left_dot ()) (right_dot (<)))
       ((lhp while_expression) (left_dot (expression "(" while))
        (right_dot (bool_operator expression ")" { program })))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term)))
       ((lhp bool_operator) (left_dot ()) (right_dot (<=)))
       ((lhp bool_operator) (left_dot ()) (right_dot (>=)))
       ((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp bool_operator) (left_dot ()) (right_dot (!=)))
       ((lhp bool_operator) (left_dot ()) (right_dot (==)))
       ((lhp expression) (left_dot (expression)) (right_dot (- term))))
      75)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp while_expression) (left_dot ("(" while))
        (right_dot (expression bool_operator expression ")" { program })))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      74)
     ((((lhp while_expression) (left_dot (while))
        (right_dot ("(" expression bool_operator expression ")" { program }))))
      73)
     ((((lhp type) (left_dot ()) (right_dot (int)))
       ((lhp program) (left_dot ()) (right_dot (program statement)))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } else {
          program } ";")))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp type) (left_dot ()) (right_dot (str)))
       ((lhp statement) (left_dot ()) (right_dot (expression ";")))
       ((lhp program) (left_dot ()) (right_dot (statement)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp statement) (left_dot ()) (right_dot (while_expression)))
       ((lhp if_expression)
        (left_dot ({ ")" expression bool_operator expression "(" if))
        (right_dot (program } else { program } ";")))
       ((lhp if_expression)
        (left_dot ({ ")" expression bool_operator expression "(" if))
        (right_dot (program } ";")))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp expression) (left_dot ()) (right_dot (set_call)))
       ((lhp assignment) (left_dot ()) (right_dot (expression = expression ";")))
       ((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp statement) (left_dot ()) (right_dot (variable_declaration ";")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp while_expression) (left_dot ())
        (right_dot
         (while "(" expression bool_operator expression ")" { program })))
       ((lhp variable_declaration) (left_dot ()) (right_dot (type id)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp statement) (left_dot ()) (right_dot (assignment)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp type) (left_dot ()) (right_dot (double)))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp statement) (left_dot ()) (right_dot (if_expression)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } ";"))))
      72)
     ((((lhp if_expression)
        (left_dot (")" expression bool_operator expression "(" if))
        (right_dot ({ program } else { program } ";")))
       ((lhp if_expression)
        (left_dot (")" expression bool_operator expression "(" if))
        (right_dot ({ program } ";"))))
      71)
     ((((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp if_expression)
        (left_dot (expression bool_operator expression "(" if))
        (right_dot (")" { program } else { program } ";")))
       ((lhp if_expression)
        (left_dot (expression bool_operator expression "(" if))
        (right_dot (")" { program } ";")))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term))))
      70)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp if_expression) (left_dot (bool_operator expression "(" if))
        (right_dot (expression ")" { program } else { program } ";")))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp if_expression) (left_dot (bool_operator expression "(" if))
        (right_dot (expression ")" { program } ";")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      69)
     ((((lhp bool_operator) (left_dot (!=)) (right_dot ()))) 68)
     ((((lhp bool_operator) (left_dot (>=)) (right_dot ()))) 67)
     ((((lhp bool_operator) (left_dot (>)) (right_dot ()))) 66)
     ((((lhp bool_operator) (left_dot (<=)) (right_dot ()))) 65)
     ((((lhp bool_operator) (left_dot (<)) (right_dot ()))) 64)
     ((((lhp bool_operator) (left_dot (==)) (right_dot ()))) 63)
     ((((lhp bool_operator) (left_dot ()) (right_dot (>)))
       ((lhp if_expression) (left_dot (expression "(" if))
        (right_dot (bool_operator expression ")" { program } ";")))
       ((lhp bool_operator) (left_dot ()) (right_dot (<)))
       ((lhp if_expression) (left_dot (expression "(" if))
        (right_dot
         (bool_operator expression ")" { program } else { program } ";")))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term)))
       ((lhp bool_operator) (left_dot ()) (right_dot (<=)))
       ((lhp bool_operator) (left_dot ()) (right_dot (>=)))
       ((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp bool_operator) (left_dot ()) (right_dot (!=)))
       ((lhp bool_operator) (left_dot ()) (right_dot (==)))
       ((lhp expression) (left_dot (expression)) (right_dot (- term))))
      62)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp if_expression) (left_dot ("(" if))
        (right_dot
         (expression bool_operator expression ")" { program } else { program }
          ";")))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp if_expression) (left_dot ("(" if))
        (right_dot (expression bool_operator expression ")" { program } ";")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      61)
     ((((lhp if_expression) (left_dot (if))
        (right_dot ("(" expression bool_operator expression ")" { program } ";")))
       ((lhp if_expression) (left_dot (if))
        (right_dot
         ("(" expression bool_operator expression ")" { program } else { program
          } ";"))))
      60)
     ((((lhp type) (left_dot (double)) (right_dot ()))) 59)
     ((((lhp type) (left_dot (str)) (right_dot ()))) 58)
     ((((lhp type) (left_dot (int)) (right_dot ()))) 57)
     ((((lhp term) (left_dot (")" expression "(")) (right_dot ()))) 56)
     ((((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term)))
       ((lhp term) (left_dot (expression "(")) (right_dot (")"))))
      55)
     ((((lhp get_call) (left_dot (")" expression , expression "(" get))
        (right_dot ())))
      54)
     ((((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp get_call) (left_dot (expression , expression "(" get))
        (right_dot (")")))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term))))
      53)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp get_call) (left_dot (, expression "(" get))
        (right_dot (expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      52)
     ((((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp get_call) (left_dot (expression "(" get))
        (right_dot (, expression ")")))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term))))
      51)
     ((((lhp set_call)
        (left_dot (")" expression , expression , expression "(" set))
        (right_dot ())))
      50)
     ((((lhp set_call) (left_dot (expression , expression , expression "(" set))
        (right_dot (")")))
       ((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term))))
      49)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp set_call) (left_dot (, expression , expression "(" set))
        (right_dot (expression ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      48)
     ((((lhp set_call) (left_dot (expression , expression "(" set))
        (right_dot (, expression ")")))
       ((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term))))
      47)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp set_call) (left_dot (, expression "(" set))
        (right_dot (expression , expression ")")))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      46)
     ((((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp set_call) (left_dot (expression "(" set))
        (right_dot (, expression , expression ")")))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term))))
      45)
     ((((lhp print_call) (left_dot (")" expression "(" print_int))
        (right_dot ())))
      44)
     ((((lhp print_call) (left_dot (expression "(" print_int)) (right_dot (")")))
       ((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term))))
      43)
     ((((lhp print_call) (left_dot (")" expression "(" print_str))
        (right_dot ())))
      42)
     ((((lhp print_call) (left_dot (expression "(" print_str)) (right_dot (")")))
       ((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term))))
      41)
     ((((lhp expression) (left_dot (term)) (right_dot ()))) 40)
     ((((lhp print_call) (left_dot (")" expression "(" print_double))
        (right_dot ())))
      39)
     ((((lhp expression) (left_dot (term % expression)) (right_dot ()))) 38)
     ((((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot (% expression)) (right_dot (term))))
      37)
     ((((lhp expression) (left_dot (term / expression)) (right_dot ()))) 36)
     ((((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot (/ expression)) (right_dot (term))))
      35)
     ((((lhp expression) (left_dot (term * expression)) (right_dot ()))) 34)
     ((((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot (* expression)) (right_dot (term))))
      33)
     ((((lhp expression) (left_dot (term - expression)) (right_dot ()))) 32)
     ((((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot (- expression)) (right_dot (term))))
      31)
     ((((lhp expression) (left_dot (term + expression)) (right_dot ()))) 30)
     ((((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot (+ expression)) (right_dot (term))))
      29)
     ((((lhp expression) (left_dot (expression)) (right_dot (+ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (- term)))
       ((lhp print_call) (left_dot (expression "(" print_double))
        (right_dot (")")))
       ((lhp expression) (left_dot (expression)) (right_dot (/ term)))
       ((lhp expression) (left_dot (expression)) (right_dot (* term)))
       ((lhp expression) (left_dot (expression)) (right_dot (% term))))
      28)
     ((((lhp expression) (left_dot (print_call)) (right_dot ()))) 27)
     ((((lhp expression) (left_dot (read_call)) (right_dot ()))) 26)
     ((((lhp expression) (left_dot (set_call)) (right_dot ()))) 25)
     ((((lhp expression) (left_dot (get_call)) (right_dot ()))) 24)
     ((((lhp term) (left_dot (const)) (right_dot ()))) 23)
     ((((lhp term) (left_dot (id)) (right_dot ()))) 22)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp print_call) (left_dot ("(" print_double))
        (right_dot (expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      21)
     ((((lhp print_call) (left_dot (print_double))
        (right_dot ("(" expression ")"))))
      20)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp print_call) (left_dot ("(" print_str)) (right_dot (expression ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      19)
     ((((lhp print_call) (left_dot (print_str)) (right_dot ("(" expression ")"))))
      18)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp print_call) (left_dot ("(" print_int)) (right_dot (expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      17)
     ((((lhp print_call) (left_dot (print_int)) (right_dot ("(" expression ")"))))
      16)
     ((((lhp read_call) (left_dot (")" "(" read_double)) (right_dot ()))) 15)
     ((((lhp read_call) (left_dot ("(" read_double)) (right_dot (")")))) 14)
     ((((lhp read_call) (left_dot (read_double)) (right_dot ("(" ")")))) 13)
     ((((lhp read_call) (left_dot (")" "(" read_str)) (right_dot ()))) 12)
     ((((lhp read_call) (left_dot ("(" read_str)) (right_dot (")")))) 11)
     ((((lhp read_call) (left_dot (read_str)) (right_dot ("(" ")")))) 10)
     ((((lhp read_call) (left_dot (")" "(" read_int)) (right_dot ()))) 9)
     ((((lhp read_call) (left_dot ("(" read_int)) (right_dot (")")))) 8)
     ((((lhp read_call) (left_dot (read_int)) (right_dot ("(" ")")))) 7)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp set_call) (left_dot ("(" set))
        (right_dot (expression , expression , expression ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      6)
     ((((lhp set_call) (left_dot (set))
        (right_dot ("(" expression , expression , expression ")"))))
      5)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp get_call) (left_dot ("(" get))
        (right_dot (expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      4)
     ((((lhp get_call) (left_dot (get))
        (right_dot ("(" expression , expression ")"))))
      3)
     ((((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp term) (left_dot ("(")) (right_dot (expression ")")))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (set_call))))
      2)
     ((((lhp type) (left_dot ()) (right_dot (int)))
       ((lhp program) (left_dot ()) (right_dot (program statement)))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } else {
          program } ";")))
       ((lhp term) (left_dot ()) (right_dot (id)))
       ((lhp type) (left_dot ()) (right_dot (str)))
       ((lhp statement) (left_dot ()) (right_dot (expression ";")))
       ((lhp program) (left_dot ()) (right_dot (statement)))
       ((lhp expression) (left_dot ()) (right_dot (print_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression + term)))
       ((lhp statement) (left_dot ()) (right_dot (while_expression)))
       ((lhp term) (left_dot ()) (right_dot (const)))
       ((lhp expression) (left_dot ()) (right_dot (expression - term)))
       ((lhp set_call) (left_dot ())
        (right_dot (set "(" expression , expression , expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression % term)))
       ((lhp expression) (left_dot ()) (right_dot (set_call)))
       ((lhp assignment) (left_dot ()) (right_dot (expression = expression ";")))
       ((lhp read_call) (left_dot ()) (right_dot (read_int "(" ")")))
       ((lhp read_call) (left_dot ()) (right_dot (read_str "(" ")")))
       ((lhp statement) (left_dot ()) (right_dot (variable_declaration ";")))
       ((lhp print_call) (left_dot ())
        (right_dot (print_double "(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (expression * term)))
       ((lhp read_call) (left_dot ()) (right_dot (read_double "(" ")")))
       ((lhp term) (left_dot ()) (right_dot ("(" expression ")")))
       ((lhp expression) (left_dot ()) (right_dot (get_call)))
       ((lhp while_expression) (left_dot ())
        (right_dot
         (while "(" expression bool_operator expression ")" { program })))
       ((lhp variable_declaration) (left_dot ()) (right_dot (type id)))
       ((lhp expression) (left_dot ()) (right_dot (term)))
       ((lhp statement) (left_dot ()) (right_dot (assignment)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_int "(" expression ")")))
       ((lhp type) (left_dot ()) (right_dot (double)))
       ((lhp expression) (left_dot ()) (right_dot (read_call)))
       ((lhp expression) (left_dot ()) (right_dot (expression / term)))
       ((lhp start) (left_dot ({)) (right_dot (program })))
       ((lhp get_call) (left_dot ())
        (right_dot (get "(" expression , expression ")")))
       ((lhp statement) (left_dot ()) (right_dot (if_expression)))
       ((lhp print_call) (left_dot ())
        (right_dot (print_str "(" expression ")")))
       ((lhp if_expression) (left_dot ())
        (right_dot
         (if "(" expression bool_operator expression ")" { program } ";"))))
      1)
     ((((lhp start) (left_dot ()) (right_dot ({ program })))
       ((lhp S') (left_dot ()) (right_dot (start))))
      0)) |}]
;;

(*TODO test it fails on invalid output bands*)
let%expect_test "test parser output works toy grammar" =
  let grammar =
    { non_terminals = [ "S"; "A" ]
    ; terminals = [ "a"; "b"; "c" ]
    ; starting_symbol = "S"
    ; productions = [ [ "S" ], [ "a"; "A" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
    }
    |> Grammar.validate
  in
  match grammar with
  | Error _ -> print_s [%sexp (grammar : Grammar.t Or_error.t)]
  | Ok grammar ->
    ((*
        abbbbc
        1 2 2 2 2 3
     *)
     let grammar = Enhanced_grammar.create grammar |> Or_error.ok_exn in
     let parser = Parser.create grammar in
     let parser_output = Parser.parse parser [ "a"; "b"; "b"; "b"; "b"; "c" ] |> ok_exn in
     print_string (Parser.Parser_output.to_string parser_output));
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
    }
    |> Grammar.validate
  in
  match grammar with
  | Error _ -> print_s [%sexp (grammar : Grammar.t Or_error.t)]
  | Ok grammar ->
    ((*
        abbbbc
        1 2 2 2 2 3
     *)
     let grammar = Enhanced_grammar.create grammar |> Or_error.ok_exn in
     let parser = Parser.create grammar in
     let parser_output = Parser.parse parser [ "a"; "b"; "b"; "b"; "b"; "c" ] |> ok_exn in
     print_string (Parser.Parser_output.to_string parser_output));
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
    }
    |> Grammar.validate
  in
  match grammar with
  | Error _ -> print_s [%sexp (grammar : Grammar.t Or_error.t)]
  | Ok grammar ->
    (let grammar = Enhanced_grammar.create grammar |> Or_error.ok_exn in
     let parser = Parser.create grammar in
     let parser_output = Parser.parse parser [ "a"; "b"; "b"; "d"; "b"; "c" ] in
     print_s [%sexp (parser_output : Parser.Parser_output.t Or_error.t)]);
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
  | Error _ -> print_s [%sexp (grammar : Grammar.t Or_error.t)]
  | Ok grammar ->
    (let grammar = Enhanced_grammar.create grammar |> Or_error.ok_exn in
     let parser = Parser.create grammar in
     let parser_output =
       Parser.parse parser [ "a"; "a"; "a"; "d"; "b"; "c"; "b"; "b" ] |> ok_exn
     in
     print_string (Parser.Parser_output.to_string parser_output));
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

let%expect_test "test p1.c works" =
  let grammar = get_language_grammar |> ok_exn |> Enhanced_grammar.create |> ok_exn in
  let parser = Parser.create grammar in
  let p1 =
    {|
    {
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
    }
  |}
  in
  let pif = scan ~program:p1 |> Or_error.ok_exn |> snd in
  let tokens = List.map ~f:fst (Pif.to_list pif) in
  let parser_output = Parser.parse parser tokens |> ok_exn in
  print_string (Parser.Parser_output.to_string parser_output);
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Goto to empty state" (state 92) })
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Or_error.ok_exn in file "src/or_error.ml", line 107, characters 17-32
  Called from Test__Compiler.(fun) in file "test/compiler.ml", line 2139, characters 3-38
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "test p2.c works" =
  let grammar = get_language_grammar |> ok_exn |> Enhanced_grammar.create |> ok_exn in
  let parser = Parser.create grammar in
  let p2 =
    {|
    {
      int n;
      n = read_int();
      int prime;
      prime = 1;
      int d;
      d = 2;
      while (d < n) {
        if (n % d == 0) {
          prime = 0;
        };
        d = d + 1;
      }
      if (prime == 0) {
        print_str("not prime");
      } else {
        print_str("prime");
      };

    }
  |}
  in
  let pif = scan ~program:p2 |> Or_error.ok_exn |> snd in
  let tokens = List.map ~f:fst (Pif.to_list pif) in
  let parser_output = Parser.parse parser tokens |> ok_exn in
  print_string (Parser.Parser_output.to_string parser_output);
  [%expect
    {|
    |   0. |      start |   1 |   - |
    |   1. |          { |   - |   2 |
    |   2. |    program |   4 |   3 |
    |   3. |          } |   - |   - |
    |   4. |    program |  46 |   5 |
    |   5. |  statement |   6 |   - |
    |   6. | if_expression |   7 |   - |
    |   7. |         if |   - |   8 |
    |   8. |          ( |   - |   9 |
    |   9. | expression |  44 |  10 |
    |  10. | bool_operator |  43 |  11 |
    |  11. | expression |  41 |  12 |
    |  12. |          ) |   - |  13 |
    |  13. |          { |   - |  14 |
    |  14. |    program |  31 |  15 |
    |  15. |          } |   - |  16 |
    |  16. |       else |   - |  17 |
    |  17. |          { |   - |  18 |
    |  18. |    program |  21 |  19 |
    |  19. |          } |   - |  20 |
    |  20. |          ; |   - |   - |
    |  21. |  statement |  22 |   - |
    |  22. | expression |  24 |  23 |
    |  23. |          ; |   - |   - |
    |  24. | print_call |  25 |   - |
    |  25. |  print_str |   - |  26 |
    |  26. |          ( |   - |  27 |
    |  27. | expression |  29 |  28 |
    |  28. |          ) |   - |   - |
    |  29. |       term |  30 |   - |
    |  30. |      const |   - |   - |
    |  31. |  statement |  32 |   - |
    |  32. | expression |  34 |  33 |
    |  33. |          ; |   - |   - |
    |  34. | print_call |  35 |   - |
    |  35. |  print_str |   - |  36 |
    |  36. |          ( |   - |  37 |
    |  37. | expression |  39 |  38 |
    |  38. |          ) |   - |   - |
    |  39. |       term |  40 |   - |
    |  40. |      const |   - |   - |
    |  41. |       term |  42 |   - |
    |  42. |      const |   - |   - |
    |  43. |         == |   - |   - |
    |  44. |       term |  45 |   - |
    |  45. |         id |   - |   - |
    |  46. |    program | 109 |  47 |
    |  47. |  statement |  48 |   - |
    |  48. | while_expression |  49 |   - |
    |  49. |      while |   - |  50 |
    |  50. |          ( |   - |  51 |
    |  51. | expression | 107 |  52 |
    |  52. | bool_operator | 106 |  53 |
    |  53. | expression | 104 |  54 |
    |  54. |          ) |   - |  55 |
    |  55. |          { |   - |  56 |
    |  56. |    program |  58 |  57 |
    |  57. |          } |   - |   - |
    |  58. |    program |  73 |  59 |
    |  59. |  statement |  60 |   - |
    |  60. | assignment |  61 |   - |
    |  61. | expression |  71 |  62 |
    |  62. |          = |   - |  63 |
    |  63. | expression |  65 |  64 |
    |  64. |          ; |   - |   - |
    |  65. | expression |  69 |  66 |
    |  66. |          + |   - |  67 |
    |  67. |       term |  68 |   - |
    |  68. |      const |   - |   - |
    |  69. |       term |  70 |   - |
    |  70. |         id |   - |   - |
    |  71. |       term |  72 |   - |
    |  72. |         id |   - |   - |
    |  73. |  statement |  74 |   - |
    |  74. | if_expression |  75 |   - |
    |  75. |         if |   - |  76 |
    |  76. |          ( |   - |  77 |
    |  77. | expression |  98 |  78 |
    |  78. | bool_operator |  97 |  79 |
    |  79. | expression |  95 |  80 |
    |  80. |          ) |   - |  81 |
    |  81. |          { |   - |  82 |
    |  82. |    program |  85 |  83 |
    |  83. |          } |   - |  84 |
    |  84. |          ; |   - |   - |
    |  85. |  statement |  86 |   - |
    |  86. | assignment |  87 |   - |
    |  87. | expression |  93 |  88 |
    |  88. |          = |   - |  89 |
    |  89. | expression |  91 |  90 |
    |  90. |          ; |   - |   - |
    |  91. |       term |  92 |   - |
    |  92. |      const |   - |   - |
    |  93. |       term |  94 |   - |
    |  94. |         id |   - |   - |
    |  95. |       term |  96 |   - |
    |  96. |      const |   - |   - |
    |  97. |         == |   - |   - |
    |  98. | expression | 102 |  99 |
    |  99. |          % |   - | 100 |
    | 100. |       term | 101 |   - |
    | 101. |         id |   - |   - |
    | 102. |       term | 103 |   - |
    | 103. |         id |   - |   - |
    | 104. |       term | 105 |   - |
    | 105. |         id |   - |   - |
    | 106. |          < |   - |   - |
    | 107. |       term | 108 |   - |
    | 108. |         id |   - |   - |
    | 109. |    program | 120 | 110 |
    | 110. |  statement | 111 |   - |
    | 111. | assignment | 112 |   - |
    | 112. | expression | 118 | 113 |
    | 113. |          = |   - | 114 |
    | 114. | expression | 116 | 115 |
    | 115. |          ; |   - |   - |
    | 116. |       term | 117 |   - |
    | 117. |      const |   - |   - |
    | 118. |       term | 119 |   - |
    | 119. |         id |   - |   - |
    | 120. |    program | 127 | 121 |
    | 121. |  statement | 122 |   - |
    | 122. | variable_declaration | 124 | 123 |
    | 123. |          ; |   - |   - |
    | 124. |       type | 126 | 125 |
    | 125. |         id |   - |   - |
    | 126. |        int |   - |   - |
    | 127. |    program | 138 | 128 |
    | 128. |  statement | 129 |   - |
    | 129. | assignment | 130 |   - |
    | 130. | expression | 136 | 131 |
    | 131. |          = |   - | 132 |
    | 132. | expression | 134 | 133 |
    | 133. |          ; |   - |   - |
    | 134. |       term | 135 |   - |
    | 135. |      const |   - |   - |
    | 136. |       term | 137 |   - |
    | 137. |         id |   - |   - |
    | 138. |    program | 145 | 139 |
    | 139. |  statement | 140 |   - |
    | 140. | variable_declaration | 142 | 141 |
    | 141. |          ; |   - |   - |
    | 142. |       type | 144 | 143 |
    | 143. |         id |   - |   - |
    | 144. |        int |   - |   - |
    | 145. |    program | 158 | 146 |
    | 146. |  statement | 147 |   - |
    | 147. | assignment | 148 |   - |
    | 148. | expression | 156 | 149 |
    | 149. |          = |   - | 150 |
    | 150. | expression | 152 | 151 |
    | 151. |          ; |   - |   - |
    | 152. |  read_call | 153 |   - |
    | 153. |   read_int |   - | 154 |
    | 154. |          ( |   - | 155 |
    | 155. |          ) |   - |   - |
    | 156. |       term | 157 |   - |
    | 157. |         id |   - |   - |
    | 158. |  statement | 159 |   - |
    | 159. | variable_declaration | 161 | 160 |
    | 160. |          ; |   - |   - |
    | 161. |       type | 163 | 162 |
    | 162. |         id |   - |   - |
    | 163. |        int |   - |   - | |}]
;;

let%expect_test "test p3.c works" =
  let grammar = get_language_grammar |> ok_exn |> Enhanced_grammar.create |> ok_exn in
  let parser = Parser.create grammar in
  let p3 =
    {|
    {
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

    }
  |}
  in
  let pif = scan ~program:p3 |> Or_error.ok_exn |> snd in
  let tokens = List.map ~f:fst (Pif.to_list pif) in
  let parser_output = Parser.parse parser tokens |> ok_exn in
  print_string (Parser.Parser_output.to_string parser_output);
  [%expect
    {|
    |   0. |      start |   1 |   - |
    |   1. |          { |   - |   2 |
    |   2. |    program |   4 |   3 |
    |   3. |          } |   - |   - |
    |   4. |    program |  15 |   5 |
    |   5. |  statement |   6 |   - |
    |   6. | expression |   8 |   7 |
    |   7. |          ; |   - |   - |
    |   8. | print_call |   9 |   - |
    |   9. |  print_int |   - |  10 |
    |  10. |          ( |   - |  11 |
    |  11. | expression |  13 |  12 |
    |  12. |          ) |   - |   - |
    |  13. |       term |  14 |   - |
    |  14. |         id |   - |   - |
    |  15. |    program |  66 |  16 |
    |  16. |  statement |  17 |   - |
    |  17. | while_expression |  18 |   - |
    |  18. |      while |   - |  19 |
    |  19. |          ( |   - |  20 |
    |  20. | expression |  64 |  21 |
    |  21. | bool_operator |  63 |  22 |
    |  22. | expression |  61 |  23 |
    |  23. |          ) |   - |  24 |
    |  24. |          { |   - |  25 |
    |  25. |    program |  27 |  26 |
    |  26. |          } |   - |   - |
    |  27. |    program |  42 |  28 |
    |  28. |  statement |  29 |   - |
    |  29. | assignment |  30 |   - |
    |  30. | expression |  40 |  31 |
    |  31. |          = |   - |  32 |
    |  32. | expression |  34 |  33 |
    |  33. |          ; |   - |   - |
    |  34. | expression |  38 |  35 |
    |  35. |          + |   - |  36 |
    |  36. |       term |  37 |   - |
    |  37. |         id |   - |   - |
    |  38. |       term |  39 |   - |
    |  39. |         id |   - |   - |
    |  40. |       term |  41 |   - |
    |  41. |         id |   - |   - |
    |  42. |    program |  55 |  43 |
    |  43. |  statement |  44 |   - |
    |  44. | assignment |  45 |   - |
    |  45. | expression |  53 |  46 |
    |  46. |          = |   - |  47 |
    |  47. | expression |  49 |  48 |
    |  48. |          ; |   - |   - |
    |  49. |  read_call |  50 |   - |
    |  50. |   read_int |   - |  51 |
    |  51. |          ( |   - |  52 |
    |  52. |          ) |   - |   - |
    |  53. |       term |  54 |   - |
    |  54. |         id |   - |   - |
    |  55. |  statement |  56 |   - |
    |  56. | variable_declaration |  58 |  57 |
    |  57. |          ; |   - |   - |
    |  58. |       type |  60 |  59 |
    |  59. |         id |   - |   - |
    |  60. |        int |   - |   - |
    |  61. |       term |  62 |   - |
    |  62. |         id |   - |   - |
    |  63. |          < |   - |   - |
    |  64. |       term |  65 |   - |
    |  65. |         id |   - |   - |
    |  66. |    program |  77 |  67 |
    |  67. |  statement |  68 |   - |
    |  68. | assignment |  69 |   - |
    |  69. | expression |  75 |  70 |
    |  70. |          = |   - |  71 |
    |  71. | expression |  73 |  72 |
    |  72. |          ; |   - |   - |
    |  73. |       term |  74 |   - |
    |  74. |      const |   - |   - |
    |  75. |       term |  76 |   - |
    |  76. |         id |   - |   - |
    |  77. |    program |  84 |  78 |
    |  78. |  statement |  79 |   - |
    |  79. | variable_declaration |  81 |  80 |
    |  80. |          ; |   - |   - |
    |  81. |       type |  83 |  82 |
    |  82. |         id |   - |   - |
    |  83. |        int |   - |   - |
    |  84. |    program |  95 |  85 |
    |  85. |  statement |  86 |   - |
    |  86. | assignment |  87 |   - |
    |  87. | expression |  93 |  88 |
    |  88. |          = |   - |  89 |
    |  89. | expression |  91 |  90 |
    |  90. |          ; |   - |   - |
    |  91. |       term |  92 |   - |
    |  92. |      const |   - |   - |
    |  93. |       term |  94 |   - |
    |  94. |         id |   - |   - |
    |  95. |    program | 102 |  96 |
    |  96. |  statement |  97 |   - |
    |  97. | variable_declaration |  99 |  98 |
    |  98. |          ; |   - |   - |
    |  99. |       type | 101 | 100 |
    | 100. |         id |   - |   - |
    | 101. |        int |   - |   - |
    | 102. |    program | 115 | 103 |
    | 103. |  statement | 104 |   - |
    | 104. | assignment | 105 |   - |
    | 105. | expression | 113 | 106 |
    | 106. |          = |   - | 107 |
    | 107. | expression | 109 | 108 |
    | 108. |          ; |   - |   - |
    | 109. |  read_call | 110 |   - |
    | 110. |   read_int |   - | 111 |
    | 111. |          ( |   - | 112 |
    | 112. |          ) |   - |   - |
    | 113. |       term | 114 |   - |
    | 114. |         id |   - |   - |
    | 115. |  statement | 116 |   - |
    | 116. | variable_declaration | 118 | 117 |
    | 117. |          ; |   - |   - |
    | 118. |       type | 120 | 119 |
    | 119. |         id |   - |   - |
    | 120. |        int |   - |   - | |}]
;;
