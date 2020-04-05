let _error name message line oc =
  Printf.fprintf oc "%s at line %d: %s" name line message;
  exit 2

let syntax_error = _error "Syntax error"

let type_error = _error "Type error"

let name_error = _error "Name error"

exception SyntaxError of string * int option
exception TypeError of string * int option
exception NameError of string * int option


let raise_syntax_error ?(line) message =
  raise (SyntaxError (message, line))
let raise_name_error ?(line) message =
  raise (NameError (message, line))
let raise_type_error ?(line) message =
  raise (TypeError (message, line))
let raise_unexpected_type_error ?(line) expected found =
  let message = "Expected an expression of type '" ^ expected ^ "' but got '" ^ found ^ "'" in
  match line with
  | Some line -> raise_type_error message ~line
  | None -> raise_type_error message
let raise_unexpected_type_error_with_name ?(line) name expected found =
  let message = "'" ^ name ^ "' has type '" ^ expected ^ "' but got '" ^ found ^ "'" in
  match line with
  | Some line -> raise_type_error message ~line
  | None -> raise_type_error message

(* Executes a function. *)
(* If an error is raised with no line, raises the same error with a line *)
let try_update_err line func =
  try func() with
  | SyntaxError (msg, None) -> raise (SyntaxError (msg, Some line))
  | TypeError (msg, None) -> raise (TypeError (msg, Some line))
  | NameError (msg, None) -> raise (NameError (msg, Some line))
  | SyntaxError _ | TypeError _ | NameError _ as error -> raise error

let try_catch oc func =
  try func () with
  | SyntaxError (msg, Some line) -> syntax_error msg line oc
  | TypeError (msg, Some line) -> type_error msg line oc
  | NameError (msg, Some line) -> name_error msg line oc
