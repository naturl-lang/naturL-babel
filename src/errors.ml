open Structures

let _error name message (location: Location.t) oc =
  Printf.fprintf oc "%s à la ligne %d: %s\n" name location.line message;
  exit 2

let syntax_error message location oc = _error "Erreur de syntaxe"  message location oc

let type_error message location oc = _error "Erreur de type" message location oc

let name_error message location oc = _error "Erreur de dénomination" message location oc

let import_error message location oc = _error "Erreur d'importation" message location oc

let bug code oc =
  Printf.fprintf oc
    "Un bug a été détecté, merci de le rapporter aux développeurs. Code d'erreur: %s.\n"
    code;
  exit 2

type error_arg = string * Location.t option
exception SyntaxError of error_arg
exception TypeError of error_arg
exception NameError of error_arg
exception ImportError of error_arg
exception Bug of string


let raise_syntax_error ?(location) message =
  raise (SyntaxError (message, location))
let raise_name_error ?(location) message =
  raise (NameError (message, location))
let raise_type_error ?(location) message =
  raise (TypeError (message, location))
let raise_import_error ?(location) message =
  raise (ImportError (message, location))
let raise_bug code =
  raise (Bug code)

let raise_unexpected_type_error ?(location) expected found =
  let message = "Une expression de type '" ^ expected ^ "' est attendue au lieu de '" ^ found ^ "'" in
  match location with
  | Some location -> raise_type_error message ~location
  | None -> raise_type_error message
let raise_unexpected_type_error_with_name ?(location) name expected found =
  let message = "'" ^ name ^ "' est du type '" ^ expected ^ "' mais le type affecté est '" ^ found ^ "'" in
  match location with
  | Some location -> raise_type_error message ~location
  | None -> raise_type_error message

(* Executes a function. *)
(* If an error is raised with no location, raises the same error with a location *)
let try_update_err location func =
  try func() with
  | SyntaxError (msg, None) -> raise (SyntaxError (msg, Some location))
  | TypeError (msg, None) -> raise (TypeError (msg, Some location))
  | NameError (msg, None) -> raise (NameError (msg, Some location))
  | ImportError (msg, None) -> raise (ImportError (msg, Some location))

let try_catch ?raise_errors oc func =
  let raise_error = raise_errors = Some true in
  try func () with
  | SyntaxError _ | TypeError _ | NameError _ | ImportError _ as error when raise_error -> raise error
  | SyntaxError (msg, Some location) -> syntax_error msg location oc
  | TypeError (msg, Some location) -> type_error msg location oc
  | NameError (msg, Some location) -> name_error msg location oc
  | ImportError (msg, Some location) -> import_error msg location oc
  | Bug code -> bug code oc

let try_execute func ~on_success ~on_failure =
  try let value = func () in on_success value with
  | SyntaxError (msg, Some location) | TypeError (msg, Some location)
  | NameError (msg, Some location) | ImportError (msg, Some location) -> on_failure (msg, location)
  | Bug code -> on_failure
                  ("Un bug a été détecté, merci de le rapporter aux développeurs. Code d'erreur: " ^ code ^ ".",
                  { line = -1; range = { start = -1; end_ = -1 } })

(* Get a list of (msg, location) corresponding to errors got by executing the function *)
let get_errors func =
  try let _ = func () in []  with
  | SyntaxError (msg, Some location) | TypeError (msg, Some location)
  | NameError (msg, Some location) | ImportError (msg, Some location) -> [ msg, location ]
