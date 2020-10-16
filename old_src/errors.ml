open Internationalisation.Translation

let _error name message line oc =
  if !lang = French then
    Printf.fprintf oc "%s à la ligne %d: %s\n" name line message
  else
    Printf.fprintf oc "%s at line %d: %s\n" name line message;
  exit 2

let syntax_error message line oc = _error (get_string SyntaxError)  message line oc

let type_error message line oc = _error (get_string TypeError) message line oc

let name_error message line oc = _error (get_string NameError) message line oc

let import_error message line oc = _error (get_string ImportError) message line oc

exception SyntaxError of string * int option
exception TypeError of string * int option
exception NameError of string * int option
exception ImportError of string * int option


let raise_syntax_error ?(line) message =
  raise (SyntaxError (message, line))
let raise_name_error ?(line) message =
  raise (NameError (message, line))
let raise_type_error ?(line) message =
  raise (TypeError (message, line))
let raise_import_error ?(line) message =
  raise (ImportError (message, line))

let raise_unexpected_type_error ?(line) expected found =
  let message = (get_string NameTypeMessage) ^ expected ^ (get_string NameButGotMessage) ^ found ^ "'" in
  match line with
  | Some line -> raise_type_error message ~line
  | None -> raise_type_error message
let raise_unexpected_type_error_with_name ?(line) name expected found =
  let message = "'" ^ name ^ (get_string HasTypeMessage) ^ expected ^ (get_string ButGotMessage) ^ found ^ "'" in
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
  | ImportError (msg, None) -> raise (ImportError (msg, Some line))
  | SyntaxError _ | TypeError _ | NameError _ | ImportError _ as error -> raise error

let try_catch ?raise_errors oc func =
  let raise_error = raise_errors = Some true in
  try func () with
  | SyntaxError _ | TypeError _ | NameError _ | ImportError _ as error when raise_error -> raise error
  | SyntaxError (msg, Some line) -> syntax_error msg line oc
  | TypeError (msg, Some line) -> type_error msg line oc
  | NameError (msg, Some line) -> name_error msg line oc
  | ImportError (msg, Some line) -> import_error msg line oc

let try_execute func ~on_success ~on_failure =
  try let value = func () in on_success value with
  | SyntaxError (msg, Some line) | TypeError (msg, Some line)
  | NameError (msg, Some line) | ImportError (msg, Some line) -> on_failure (msg, line)

(* Get a list of (msg, line) corresponding to errors got by executing the function *)
let get_errors func =
  try let _ = func () in []  with
  | SyntaxError (msg, Some line) | TypeError (msg, Some line)
  | NameError (msg, Some line) | ImportError (msg, Some line) -> [ msg, line ]
