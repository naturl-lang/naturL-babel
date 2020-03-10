open Printf
open Dependencies.Translation
open Dependencies.Structures

let vars = VarSet.empty

let%expect_test _ = let context = {code = "si 2 > 3 alors\nfin"; index = 0; vars; scopes = [If]; imports = []} in
  let translation, _ = eval_si context in
  printf "%s" translation;
  [%expect "if 2 > 3:"] ;;

print_endline "Test Finished" ;;
