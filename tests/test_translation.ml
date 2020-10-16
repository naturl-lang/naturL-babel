open Old_src.Utils
open Old_src.Translation
open Old_src.Structures ;;

print_string "Beginning translation.ml tests... " ;;

let filename = "<stdin>"
let vars = StringMap.empty ;;
let max_index = None ;;
let defs = StringMap.empty ;;

let%expect_test "fonction" = let context = {filename; code = "fonction test() -> entier\ndebut\nretourner 5\nfin"; index = 0; max_index; vars; defs; scopes = [Function_definition ""]} in
  let translation, _ = eval_fonction context in
  print_string translation;
  [%expect {|
    def test():
        return 5|}] ;;

let%expect_test "procedure" = let context = {filename; code = "procedure test()\ndebut\nfin"; index = 0; max_index; vars; defs; scopes = [Function_definition ""]} in
  let translation, _ = eval_procedure context in
  print_string translation;
  [%expect {|
    def test():
        pass |}] ;;

let%expect_test "si" = let context = {filename; code = "si 2 < 3 alors\nfin"; index = 0; max_index; vars; defs; scopes = [If 0]} in
  let translation, _ = eval_si context in
  print_string translation;
  [%expect {|
    if True:
        pass |}]
;;

let%expect_test "si vide" = let context = {filename; code = "si 2 > 3 alors\nfin"; index = 0; max_index; vars; defs; scopes = [If 0]} in
  let translation, _ = eval_si context in
  print_string translation;
  [%expect {| |}]
;;

let%expect_test "sinon_si" = let context = {filename; code = "sinon_si vrai alors\nfin"; index = 0; max_index; vars; defs; scopes = [If 0]} in
  let translation, _ = eval_sinon_si context in
  print_string translation;
  [%expect {|
    elif True:
        pass |}] ;;

let%expect_test "sinon" = let context = {filename; code = "sinon\nfin"; index = 0; max_index; vars; defs; scopes = [If 0]} in
  let translation, _ = eval_sinon context in
  print_string translation;
  [%expect {|
    else:
        pass |}] ;;

let%expect_test "tant_que" = let context = {filename; code = "tant_que vrai faire\nfin"; index = 0; max_index; vars; defs; scopes = [While]} in
  let translation, _ = eval_tant_que context in
  print_string translation;
  [%expect {|
    while True:
        pass|}] ;;

let%expect_test "pour" = let vars = StringMap.add "n" `Int (StringMap.add "i" `Int vars) in
  let translation, _ = eval_pour {filename; code = "pour i de 1 jusqu_a n faire\nfin"; index = 0; max_index; vars; defs; scopes = [For]} in
  print_string translation;
  [%expect {|
    for i in range(1, n + 1):
        pass|}] ;;

let%expect_test "pour_chaque" = let vars = StringMap.add "c" `Char (StringMap.add  "str" `String vars) in
  let translation, _ = eval_pour_chaque {filename; code = "pour_chaque c dans str faire\nfin"; index = 0; max_index; vars; defs; scopes = [For]} in
  print_string translation;
  [%expect {|
    for c in str:
        pass |}] ;;

let%expect_test "product" = let code = {|
fonction multiplication(entier a, entier b) -> entier
variables
  entier i, total
debut
  total <- 0
  pour i de 1 jusqu_a b faire
    total <- total + a
  fin

  retourner total
fin
|}
  in print_string (translate_code filename code);
  [%expect {|
    def multiplication(a, b):
        total = 0
        for i in range(1, b + 1):
            total = total + a
        return total |}] ;;

print_endline "Done." ;;
