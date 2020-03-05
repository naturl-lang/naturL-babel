open Dependencies.Expressions

let () =
  let str = ref "" in
  for i = 1 to Array.length Sys.argv - 1 do
    str := !str ^ Sys.argv.(i)
  done;
  let expr = expr_of_str !str in
  print_endline (tree_of_expr expr)
