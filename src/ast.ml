open Errors
open Structures
open Context
open Expressions

(* The first parameter is always the line number *)
type t =
  | Body of Location.t * t list
  | Expr of Location.t * Expr.t
  | Return of Location.t * Expr.t
  | Assign of Location.t * string * Expr.t
  | If of Location.t * Expr.t * t * t option  (* target, body, else *)
  | Else of Location.t * t
  | For of Location.t * string * Expr.t * Expr.t * t
  | For_each of Location.t * string * Expr.t * t
  | While of Location.t * Expr.t * t
  | Func_definition of Location.t * string * (string * string) list * string * t
  | End

let make_body ~children ~location =
  Body (location, children)
let make_expr ~expr ~context ~location =
  assert_not_in_func_def ~context;
  Expr (location, expr)
let make_return ~expr ~context ~location =
  assert_not_in_func_def ~context;
  if is_in_function context then
    Return (location, expr)
  else
    raise_syntax_error ~location
      "Le mot-clé 'retourner' ne peut pas être présent en dehors d'une fonction"
let make_assign ~target ~value ~context ~location =
  assert_not_in_func_def ~context;
  Assign (location, target, value)
let make_if ~target ~body ~context ~location =
  assert_not_in_func_def ~context;
  let rec extract_else body body_location = function
    | [] -> make_body ~children:body ~location, None
    | Else (_, else_body) :: [] ->
      make_body ~children:body ~location:body_location, Some else_body
    | child :: tail -> extract_else (child :: body) body_location tail
  in
  match body with
  | Body (body_location, children) ->
    let body, else_ = extract_else [] body_location children
    in If (location, target, body, else_)
  | node -> If (location, target, node, None)
let make_else_if ~target ~body ~context ~location =
  assert_not_in_func_def ~context;
  if is_in_if context then
    Else (location, make_if ~target ~body ~context ~location)
  else
    raise_syntax_error ~location
      "Un bloc 'sinon_si' doit être précédé d'un bloc 'si' ou 'sinon_si'"
let make_else ~body ~context ~location =
  assert_not_in_func_def ~context;
  if is_in_if context then
    Else (location, body)
  else
    raise_syntax_error ~location
      "Un bloc 'sinon' doit être précédé d'un bloc 'si' ou 'sinon_si'"
let make_for ~target ~start ~end_ ~body ~context ~location =
  assert_not_in_func_def ~context;
  For (location, target, start, end_, body)
let make_for_each ~target ~iter ~body ~context ~location =
  assert_not_in_func_def ~context;
  For_each (location, target, iter, body)
let make_while ~test ~body ~context ~location =
  assert_not_in_func_def ~context;
  While (location, test, body)
let make_func_definition ~name ~args ~ret_type ~body ~context ~location =
  assert_not_in_func_def ~context;
  Func_definition (location, name, args, ret_type, body)
let make_end () = End

(*********************************

let print_ast ast =
  let print_indent depth =
    print_string (String.make (depth * 2) ' ')
  in
  let rec print_ast depth ast =
    print_indent depth;
    match ast with
    | Body children ->
      children |> List.iter @@ print_ast depth
    | Expr _ -> print_endlocation "expression"
    | Return _ -> print_endlocation "RETOURNER expression"
    | Assign (target, _) -> print_endlocation (target ^ " <- expression")
    | If (_, body, or_else) ->
      print_endlocation "SI";
      print_ast (depth + 1) body;
      print_indent (depth + 1); print_endlocation "SINON";
      (match or_else with Some or_else -> print_ast (depth + 1) or_else | None -> ())
    | Else body ->
      print_endlocation "SINON";
      print_ast (depth + 1) body
    | For (variable, _, _, body) ->
      print_endlocation "POUR";
      print_indent (depth + 1); print_endlocation ("VARIABLE: " ^ variable);
      print_ast (depth + 1) body
    | For_each (variable, _, body) ->
      print_endlocation "POUR CHAQUE";
      print_indent (depth + 1); print_endlocation ("VARIABLE: " ^ variable);
      print_ast (depth) body
    | While (_, body) ->
      print_endlocation "TANT QUE";
      print_ast (depth + 1) body
    | Func_definition (name, args, ret_type, body) ->
      print_endlocation "FONCTION";
      print_indent (depth + 1); print_endlocation ("NOM: " ^ name);
      print_indent (depth + 1); print_endlocation "ARGUMENTS"; args |> List.iter (fun (type_, name) ->
          print_indent (depth + 1);
          print_endlocation (name ^ " de type " ^ type_));
      print_indent (depth + 1); print_endlocation ("RETURN TYPE: " ^ ret_type);
      print_ast (depth + 1) body
    | End -> print_endlocation "FIN"
  in print_newlocation (); print_ast 0 ast
*)
