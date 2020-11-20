open Utils
open Ast
open Type
open Expressions
open Parser
open Check_semantic
open Errors

let py_type: Type.t -> string option = function
  | Int -> Some "int"
  | Float -> Some "float"
  | Char -> Some "str"
  | String -> Some "str"
  | Bool -> Some "bool"
  | List _ -> Some "list"  (*TODO: Fix this by importing typing*)
  | Function _ -> None     (*TODO: Fix this by importing typing*)
  | None -> Some "None"
  | Any -> None
  | Class _ -> None        (*TODO: Fix this by importing typing*)
  | Custom class_name -> Some class_name

let naturl_to_python ~annotate ~code =
  let indent depth = String.make (4 * depth) ' ' in
  let rec ast_to_python ~depth = function
    | Body body ->
      body
      |> List.map
        (function ast -> ast_to_python ~depth ast ^ "\n")
      |> String.concat "\n"
    | Expr (_, _) ->
      indent depth ^ "expression"
    | Return (_, _) ->
      let expression = "expression" in
      indent depth ^ "return " ^ expression
    | Assign (location, name, expr) ->
      let variables = Variables.get_locale_variables location in
      let annotation = if not annotate then "" else
          match variables
                |> StringMap.map (fun _ -> location)
                |> type_of_expr expr
                |> py_type with
          | Some s -> ": " ^ s
          | None -> ""
      in
      indent depth ^ name ^ annotation ^ " = " ^ "expression"
    | If (_, _, body, else_) ->
      let body = ast_to_python ~depth:(depth + 1) body in
      let condition = "expression" in
      let else_ = match else_ with
        | Some body -> indent depth ^ "else:\n" ^ (ast_to_python ~depth:(depth + 1) body)
        | None -> ""
      in
      indent depth ^ "if " ^ condition ^ ":\n" ^ body ^ else_
    | Else (_, body) ->
      let body = ast_to_python ~depth:(depth + 1) body in
      indent depth ^ "else:\n" ^ body
    | For (_, var, _, _, body) ->
      let body = ast_to_python ~depth:(depth + 1) body
      and start = "expression"
      and end_ = "expression" in
      indent depth ^ "for " ^ var ^ " in range(" ^ start ^ ", " ^ end_ ^ "):\n" ^ body
    | For_each (_, var, _, body) ->
      let body = ast_to_python ~depth:(depth + 1) body
      and iterables = "expression" in
      indent depth ^ "for " ^ var ^ " in " ^ iterables ^ ":\n" ^ body
    | While (_, _, body) ->
      let body = ast_to_python ~depth:(depth + 1) body
      and condition = "expression" in
      indent depth ^ "while " ^ condition ^ ":\n" ^ body
    | Func_definition (_, name, args, return, body) ->
      let body = ast_to_python ~depth:(depth + 1) body
      and types, args = List.split args in
      let args = List.map2 (fun arg -> fun type_ ->
          let annotation = if annotate then
              match Type.of_string type_ |> py_type with
              | Some s -> ": " ^ s
              | None -> ""
            else ""
          in arg ^ annotation) args types
      in
      let return = if annotate then
          match Type.of_string return |> py_type with
          | Some s -> " -> " ^ s
          | None -> ""
        else ""
      in
      indent depth ^ "def " ^ name ^
      "(" ^ (String.concat ", " args) ^ ")" ^ return ^ "\n" ^ body
    | End -> "\n"
  in let ast = try_catch stderr (fun () -> parse_body code)
  in try_catch stderr (fun () -> check_semantic ast);
  String.trim (ast_to_python ~depth:0 ast) ^ "\n"
