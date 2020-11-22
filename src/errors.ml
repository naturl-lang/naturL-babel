open Structures

let _error name message location oc =
  Printf.fprintf oc "%s à la %s: %s\n" name (Location.to_string_fr location) message;
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

let raise_function_error ?location name args user_args =
  let n = List.length args in
  let m = List.length user_args in
  let dash = if n > 1 then "- " else "" in
  let arg (name, type_) =
    dash ^ name ^ ", de type '" ^ type_ ^ "'"
  in
  let user_arg type_ =
    "'" ^ type_ ^ "'"
  in
  let rec args_list = function
    | [] -> ""
    | h :: [] -> arg h
    | h :: t -> arg h ^ "\n" ^ args_list t
  in
  let rec user_args_list = function
    | [] -> ""
    | t :: [] -> user_arg t
    | t1 :: t2 :: [] -> user_arg t1 ^ " et " ^ user_arg t2
    | h :: t -> user_arg h ^ ", " ^ user_args_list t
  in
  (* User prototype *)
  let error = if m = 0 then
      "La fonction '" ^ name ^ "' est appelée sans arguments.\n"
    else
      let param = if m > 1 then " arguments de types " else " argument de type " in
      "La fonction '" ^ name ^ "' est appelée avec " ^ string_of_int m ^ param ^ user_args_list user_args ^ ".\n"
  in
  (* Expected prototype *)
  let error = error ^ if n = 0 then
      "Elle n'attend d'arguments"
    else
      let param = if n > 1 then " arguments :\n" else " argument : " in
      "Elle attend " ^ string_of_int n ^ param ^ args_list args
  in match location with
  | Some location -> raise_type_error ~location error
  | None -> raise_type_error error

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
                  ("Un bug a été détecté, merci de le rapporter aux développeurs. Code d'erreur: " ^
                   code ^ ".",
                  { line = -1; range = { start = -1; end_ = -1 } })

(* Allow a function to raise multiple exceptions *)
type error = {
  header: string;
  message: string;
  location: Location.t;
}

let errors = Queue.create ()

let clear_errors () =
  Queue.clear errors

let get_errors () =
  errors
  |> Utils.list_of_queue
  |> List.map (function { header = _; message; location } -> message, location)

let make_error_header = function
  | SyntaxError (_, Some location) -> "Erreur de syntaxe à la " ^ Location.to_string_fr location
  | TypeError (_, Some location) -> "Erreur de type à la " ^ Location.to_string_fr location
  | NameError (_, Some location) -> "Erreur de nommage à la " ^ Location.to_string_fr location
  | ImportError (_, Some location) -> "Erreur d'importation à la " ^ Location.to_string_fr location
  | _ -> assert false

let try_add_error func ~default =
  try func () with
  | SyntaxError (msg, Some location) | TypeError (msg, Some location)
  | NameError (msg, Some location) | ImportError (msg, Some location) as e ->
    let header = make_error_header e ^ " : "
    and message = msg in
    Queue.add { header; message; location } errors;
    default

let try_print_errors () =
  errors
  |> Queue.iter (function { header; message; location = _ } ->
      prerr_endline (header ^ message));
  if not @@ Queue.is_empty errors then
    exit 2
