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

type tt = {
  operators : string list;
  separators : string list;
  reserved_words : string list;
}
[@@deriving sexp]

let create () =
  {
    operators =
      [ "+"; "-"; "*"; "/"; "%"; "=="; "<="; "<"; ">="; ">"; "="; "!=" ];
    separators = [ "{"; "}"; "("; ")"; ";"; " "; "$" ];
    reserved_words =
      [
        "int";
        "str";
        "double";
        "if";
        "else";
        "while";
        "get";
        "set";
        "read_int";
        "read_str";
        "read_double";
        "print_int";
        "print_str";
        "print_double";
      ];
  }

let%expect_test "" =
  let tt = create () in
  print_s [%message (tt : tt)];
  [%expect
    {|
    (tt
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
  let tt = tt_of_sexp (Sexp.of_string sexp) in
  print_s [%message (tt : tt)];
  [%expect
    {|
    (tt
     ((operators (+ - * / % == <= < >= > = !=))
      (separators ({ } "(" ")" ";" " " $))
      (reserved_words
       (int str double if else while get set read_int read_str read_double
        print_int print_str print_double)))) |}]

let scan =
  let operators =
    "[+]|[-]|[*]|[/]|[%]|[=][=]|[<][=]|[<]|[>][=]|[>]|[=]|[!][=]"
  in
  let separators = "[{]|[}]|[(]|[)]|[;]|[ ]|$" in
  (* let separators = "[{}(); ]|$" in *)
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

let%expect_test "test scanner easy input" =
  let result = scan ~program:"int a;" in
  let st, pif = Or_error.ok_exn result in
  let pif = Pif.to_list pif in
  print_s [%message (pif : (string * int) list)];
  [%expect {|
    (pif ((int -1) (id 97) (";" -1))) |}];
  print_s [%message (Symbol_table.get_symbol st 97 : string option)];
  [%expect {| ("Symbol_table.get_symbol st 97" (a)) |}]

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
  let pif = Pif.to_list pif in
  print_s [%message (pif : (string * int) list)];
  [%expect
    {|
    (pif
     ((int -1) (id 97) (";" -1) (int -1) (id 98) (";" -1) (int -1) (id 99)
      (";" -1) (id 97) (= -1) (read_int -1) ("(" -1) (")" -1) (";" -1) (id 98)
      (= -1) (read_int -1) ("(" -1) (")" -1) (";" -1) (id 99) (= -1)
      (read_int -1) ("(" -1) (")" -1) (";" -1) (int -1) (id 441021) (";" -1)
      (id 441021) (= -1) (id 97) (";" -1) (if -1) ("(" -1) (id 98) (> -1)
      (id 441021) (")" -1) ({ -1) (id 441021) (= -1) (id 98) (} -1) (if -1)
      ("(" -1) (id 99) (> -1) (id 441021) (")" -1) ({ -1) (id 441021) (= -1)
      (id 99) (";" -1) (} -1) (print_int -1) ("(" -1) (id 441021) (")" -1)
      (";" -1))) |}];
  print_string (Symbol_table.to_hum st);
  [%expect {|
    97: a
    98: b
    99: c
    441021: ans |}]

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
  let pif = Pif.to_list pif in
  print_s [%message (pif : (string * int) list)];
  [%expect
    {|
    (pif
     ((int -1) (id 110) (";" -1) (id 110) (= -1) (read_int -1) ("(" -1) (")" -1)
      (";" -1) (int -1) (id 196883) (";" -1) (id 196883) (= -1) (const 49)
      (";" -1) (int -1) (id 100) (";" -1) (id 100) (= -1) (const 50) (";" -1)
      (while -1) ("(" -1) (id 100) (< -1) (id 110) (")" -1) ({ -1) (if -1)
      ("(" -1) (id 110) (% -1) (id 100) (== -1) (const 48) (")" -1) ({ -1)
      (id 196883) (= -1) (const 48) (";" -1) (} -1) (id 100) (= -1) (id 100)
      (+ -1) (const 49) (";" -1) (} -1) (if -1) ("(" -1) (id 196883) (== -1)
      (const 48) (")" -1) ({ -1) (print_str -1) ("(" -1) (const 649405) (")" -1)
      (} -1) (else -1) ({ -1) (print_str -1) ("(" -1) (const 483306) (")" -1)
      (} -1))) |}];
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
  let pif = Pif.to_list pif in
  print_s [%message (pif : (string * int) list)];
  [%expect
    {|
    (pif
     ((int -1) (id 110) (";" -1) (id 110) (= -1) (read_int -1) ("(" -1) (")" -1)
      (";" -1) (int -1) (id 105) (";" -1) (id 105) (= -1) (const 48) (";" -1)
      (int -1) (id 299670) (";" -1) (id 299670) (= -1) (const 48) (";" -1)
      (while -1) ("(" -1) (id 105) (< -1) (id 110) (")" -1) ({ -1) (int -1)
      (id 120) (";" -1) (id 120) (= -1) (read_int -1) ("(" -1) (")" -1) (";" -1)
      (id 299670) (= -1) (id 299670) (+ -1) (id 120) (";" -1) (} -1)
      (print_int -1) ("(" -1) (id 299670) (")" -1) (";" -1))) |}];
  print_string (Symbol_table.to_hum st);
  [%expect {|
    48: 0
    105: i
    110: n
    120: x
    299670: sum |}]

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
