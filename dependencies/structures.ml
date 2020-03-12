open Errors

module Type =
struct
  type type_struct =
    | Int
    | Float
    | Char
    | String
    | Boolean
    | List of type_struct
    | Function
    | None

  let rec string_of_type = function
      Int -> "entier"
    | Float -> "reel"
    | Char -> "caractere"
    | String -> "chaine"
    | Boolean -> "boolean"
    | List t -> "liste de " ^ string_of_type t
    | Function -> "fonction"
    | None -> "rien"

  let rec type_of_string = function
      "entier" -> Int
    | "reel" -> Float
    | "caractere" -> Char
    | "chaine" -> String
    | "booleen" -> Boolean
    | "liste" -> List Int
    | "fonction" -> Function
    | "rien" -> None
    | str -> (match String.split_on_char ' ' str with
      | "liste" :: "de" :: t -> List (type_of_string (String.concat " " t))
      | _ -> raise_name_error ("Unknown type " ^ str))

  let get_iterable_type = function
      String -> Some Char
    | List t -> Some t
    | _ -> None
end

type variable = {name: string; type_struct: Type.type_struct}

module VarSet = Set.Make(struct type t = variable let compare = compare end)

let print_variable var =
  print_endline ("var " ^ var.name ^ ": " ^ Type.string_of_type var.type_struct)

let print_var_set set =
  let rec print_var_list = function
      [] -> print_endline "}"
    | var :: t ->
      print_char '\t';
      print_variable (var);
      print_var_list t
  in print_endline "{"; print_var_list (VarSet.elements set)

type expr =
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Equal of expr * expr
  | Greater of expr * expr
  | GreaterOrEqual of expr * expr
  | Lower of expr * expr
  | LowerOrEqual of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Value of Type.type_struct

type scope =
  | If
  | Else
  | While
  | For
  | Function

(* The current context of the code *)
type context = {
  code: string;          (* The whole code *)
  index: int;            (* The index *)
  vars: VarSet.t;        (* The set of known variables *)
  scopes: scope list;   (* The stack of scopes *)
  imports: string list;  (* The list of imports that need to be added at the beginning of the code *)
}
