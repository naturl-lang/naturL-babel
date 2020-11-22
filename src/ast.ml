open Errors
open Structures
open Context
open Expressions

(* The first parameter is always the line number *)
type t =
  | Body of t list
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

let make_body ~children =
  Body children
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
  let rec extract_else body = function
    | [] -> make_body ~children:body, None
    | Else (_, else_body) :: [] ->
      make_body ~children:body, Some else_body
    | child :: tail -> extract_else (child :: body) tail
  in
  match body with
  | Body children ->
    let body, else_ = extract_else [] children
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
  if not @@ Variables.is_var_defined target then
    ();
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
