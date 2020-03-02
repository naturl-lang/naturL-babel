open Dependencies.Expressions

let () =
  let str = ref "" in
  for i = 1 to Array.length Sys.argv - 1 do
    str := !str ^ Sys.argv.(i)
  done;
  let expr = str_to_expr !str in
  print_endline (string_of_expr expr)
