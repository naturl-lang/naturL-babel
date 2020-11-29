let rec simplify ast =
  match ast with
  |Ast.Body l -> Ast.Body (List.map simplify l)
  |Ast.If (location, condition, body, else_) ->
    (match condition with
    |Value (Bool true) -> simplify body
    |Value (Bool false) -> Option.value (simplify_opt else_) ~default:(Ast.Body [])
    |_ -> Ast.If (location, condition, simplify body, simplify_opt else_))
  |Ast.While (location, condition, body) ->
    (match condition with
    |Value (Bool false) -> Body []
    |_ -> Ast.While (location, condition, simplify body))
  |_ -> ast
and simplify_opt opt = match opt with
  |None -> None
  |Some option -> Some (simplify option)
