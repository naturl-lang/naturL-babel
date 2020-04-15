open Src.Utils
open Src.Translation
open Src.Structures ;;

print_string "Beginning translation.ml tests... " ;;

let vars = StringMap.empty ;;

let%expect_test "fonction" = let context = {code = "fonction test() -> entier\ndebut\nretourner 5\nfin"; index = 0; vars; scopes = [Function ""]} in
  let translation, _ = eval_fonction context in
  print_string translation;
  [%expect {|
    def test():
        return 5|}] ;;

let%expect_test "procedure" = let context = {code = "procedure test()\ndebut\nfin"; index = 0; vars; scopes = [Function ""]} in
  let translation, _ = eval_procedure context in
  print_string translation;
  [%expect {|
    def test():
        pass |}] ;;

let%expect_test "si" = let context = {code = "si 2 > 3 alors\nfin"; index = 0; vars; scopes = [If]} in
  let translation, _ = eval_si context in
  print_string translation;
  [%expect {|
    if False:
        pass |}]
;;

let%expect_test "sinon_si" = let context = {code = "sinon_si vrai alors\nfin"; index = 0; vars; scopes = [If]} in
  let translation, _ = eval_sinon_si context in
  print_string translation;
  [%expect {|
    elif True:
        pass |}] ;;

let%expect_test "sinon" = let context = {code = "sinon\nfin"; index = 0; vars; scopes = [Else]} in
  let translation, _ = eval_sinon context in
  print_string translation;
  [%expect {|
    else:
        pass |}] ;;

let%expect_test "tant_que" = let context = {code = "tant_que vrai faire\nfin"; index = 0; vars; scopes = [While]} in
  let translation, _ = eval_tant_que context in
  print_string translation;
  [%expect {|
    while True:
        pass|}] ;;

let%expect_test "pour" = let vars = StringMap.add "n" `Int (StringMap.add "i" `Int vars) in
  let translation, _ = eval_pour {code = "pour i de 1 jusqu_a n faire\nfin"; index = 0; vars; scopes = [For]} in
  print_string translation;
  [%expect {|
    for i in range(1, n + 1):
        pass|}] ;;

let%expect_test "pour_chaque" = let vars = StringMap.add "c" `Char (StringMap.add  "str" `String vars) in
  let translation, _ = eval_pour_chaque {code = "pour_chaque c dans str faire\nfin"; index = 0; vars; scopes = [For]} in
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
  in print_string (translate_code code);
  [%expect {|
    def multiplication(a, b):
        total = 0
        for i in range(1, b + 1):
            total = total + a
        return total |}] ;;

print_endline "Done." ;;
