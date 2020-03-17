open Dependencies.Getters
open Dependencies.Structures ;;

print_string "Beginning getters.ml tests... " ;;

let%expect_test "ignore_chrs" =
  print_int (ignore_chrs " ()\n(abc)" 0);
  [%expect {| 5 |}] ;;

let%expect_test "get_word" =
  let word, index = get_word "  (Hello, world!)" 0 in
  print_string word;
  [%expect {| Hello |}];
  print_int index;
  [%expect {| 9 |}] ;;

let%expect_test "get_expression" =
  let expr, index = get_expression "abab abaa aabb abba aab" 0 "abba" in
  print_string expr;
  [%expect {| abab abaa aabb |}];
  print_int index;
  [%expect {| 20 |}]

let%expect_test "get_line" =
  let line, index = get_line "first line\nsecond line" 0 in
  print_string line;
  [%expect {| first line |}];
  print_int index;
  [%expect {| 11 |}] ;;

let%expect_test "get_param" =
  let params, index, vars = get_param StringMap.empty "fonction test(entier n, reel pi) -> entier" 14 in
  print_string params;
  [%expect {| n, pi |}];
  print_int index;
  [%expect {| 33 |}];
  print_vars vars;
  [%expect {|
var n : entier
var pi : reel
|}] ;;

let %expect_test "get_line_no" =
  let code = "First line\nSecond line\nThird line\n" in
  print_int (get_line_no code 4);
  [%expect {| 1 |}];
  print_int (get_line_no code 16);
  [%expect {| 2 |}];
  print_int (get_line_no code 30);
  [%expect {| 3 |}] ;;

print_endline "Done." ;;
