open Dependencies.Translation
open Dependencies.Structures

let vars = VarSet.empty

let%expect_test _ = let context = {code = "si 2 > 3 alors\nfin"; index = 0; vars; scopes = [If]; imports = []} in
  let translation, _ = eval_si context in
  print_string translation;
  [%expect {|
   if 2 > 3:
       pass |}]
;;

print_endline "Test Finished" ;;
