open Errors
open Getters

type scope =
  | If
  | Else
  | Else_if
  | For
  | While
  | Func of bool  (*true if the function is defined*)

type context =
  {
      code: string;
     index: int;
    scopes: scope list;
  }

(**************************************)

let is_in_func_definition context =
  List.exists (fun s -> s = Func false) context.scopes

let is_in_function context =
  List.exists (fun s -> s = Func true) context.scopes

let mark_func_defined context =
  let rec _mark_func = function
    | Func false :: t -> Func true :: t
    | h :: t -> h :: _mark_func t
    | [] -> []
  in { context with scopes = _mark_func context.scopes }

let is_in_if context =
  match context.scopes with
  | If :: _ -> true
  | _ -> false

(**************************************)

let assert_not_in_func_def ~context =
  if is_in_func_definition context then
    raise_syntax_error ~location:(get_current_line_location context.code context.index)
      "Mot-clé 'debut' attendu dans la défnition d'une fonction"
