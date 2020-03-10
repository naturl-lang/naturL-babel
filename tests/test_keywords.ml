open Dependencies.Keywords
open Dependencies.Structures

let vars = VarSet.empty

let%test _ = eval_si vars "si 2 > 3 alors"  0 "" = ("if 2 > 3:\n", 14, vars);;

print_endline "Test Finished";;

