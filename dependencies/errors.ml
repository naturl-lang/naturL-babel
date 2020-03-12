let _error name message line oc =
  Printf.fprintf oc "%s at line %d: %s" name line message;
  exit 2

let syntax_error = _error "Syntax error"

let type_error = _error "Type error"

let name_error = _error "Name error"

exception SyntaxError of string
exception TypeError of string
exception NameError of string

let raise_unexpected_type_error expected found =
  raise (TypeError ("Expected an expression of type '" ^ expected ^ "' but got '" ^ found ^ "'"))

let translate_exception exc =
  match exc with
  | SyntaxError msg -> syntax_error msg
  | TypeError msg -> type_error msg
  | NameError msg -> name_error msg
  | _ -> raise exc
