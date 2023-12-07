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
  (* TODO: use [create] instead *)
  let grammar : Grammar.t =
    { non_terminals = [ "S"; "A" ]
    ; terminals = [ "a"; "b"; "c" ]
    ; starting_symbol = "S" (* TODO: validate productions in the create *)
    ; productions = [ [ "S" ], [ "a"; "A" ]; [ "A" ], [ "b"; "A" ]; [ "A" ], [ "c" ] ]
    }
  in
  let grammar = Enhanced_grammar.create grammar |> Or_error.ok_exn in
  Parser.create grammar
;;

let%expect_test "TODO [Grammar.create]" = ()

let%expect_test "test closure on empty set" =
  let parser = get_parser () in
  let items = Hash_set.of_list (module Parser.Lr0_item) [] in
  let closure = Parser.closure parser items in
  print_string (Parser.State.to_string_hum closure);
  [%expect {|
    () |}]
;;

let%expect_test "test canonical collection basic" =
  let parser = get_parser () in
  let cannonical_collection =
    Parser.get_cannonical_collection parser |> Or_error.ok_exn
  in
  print_string
    (String.concat
       ~sep:"\n"
       (List.map cannonical_collection ~f:Parser.State.to_string_hum));
  [%expect
    {|
      ("[S' -> S.]")
      ("[S -> a A.]")
      ("[A -> b A.]")
      ("[A -> c.]")
      ("[A -> .c]""[A -> b.A]""[A -> .b A]")
      ("[S -> a.A]""[A -> .c]""[A -> .b A]")
      ("[S' -> .S]""[S -> .a A]") |}]
;;
