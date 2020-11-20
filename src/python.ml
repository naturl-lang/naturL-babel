open Utils
open Ast
open Type
open Expressions
open Parser
open Check_semantic
open Errors

(********************* Type *********************)

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

(********************* Expression *********************)

let py_expr =
  let rec py_expr (parent: Expr.t option) (expression: Expr.t) =
    let string, expr = match expression with
    | Plus (left, right) as expr -> binary_op " + " expr left right, expr
    | Minus (left, right) as expr -> binary_op " - " expr left right, expr
    | Times (left, right) as expr -> binary_op " * " expr left right, expr
    | Div (left, right) as expr -> binary_op " / " expr left right, expr
    | Div_int (left, right) as expr -> binary_op " // " expr left right, expr
    | Modulus (left, right) as expr -> binary_op " % " expr left right, expr
    | Pow (left, right) as expr -> binary_op " ** " expr left right, expr
    | Neg arg as expr -> "-" ^ py_expr (Some expr) arg, expr
    | Eq (left, right) as expr -> binary_op " == " expr left right, expr
    | Gt (left, right) as expr -> binary_op " > " expr left right, expr
    | Lt (left, right) as expr -> binary_op " < " expr left right, expr
    | Gt_eq (left, right) as expr -> binary_op " >= " expr left right, expr
    | Lt_eq (left, right) as expr -> binary_op " <= " expr left right, expr
    | And (left, right) as expr -> binary_op " and " expr left right, expr
    | Or (left, right) as expr -> binary_op " or " expr left right, expr
    | Not arg as expr -> "not" ^ py_expr (Some expr) arg, expr
    | List l as expr ->
      "[" ^ String.concat ", " (List.map (py_expr None) l) ^ "]", expr
    | Call (name, args) as expr ->
      name ^ "(" ^ String.concat ", " (List.map (py_expr None) args) ^ ")", expr
    | Subscript (arg, index) as expr ->
      py_expr (Some expr) arg ^ "[" ^ py_expr None index ^ "]", expr
    | Value value as expr ->
      begin
        match value with
        | Int int -> Big_int.string_of_big_int int
        | Float float -> string_of_float float
        | Char char -> "'" ^ String.make 1 char ^ "'"
        | String string -> {|"|} ^ string ^ {|"|}
        | Bool bool -> if bool then "True" else "False"
        | Variable name -> name
        | Instance _ -> assert false
        | None -> "None"
      end, expr
    | Access (var, attr) as expr -> py_expr (Some expr) var ^ "." ^ attr, expr
    in match parent with
    | Some parent when precedence (expr_to_op parent) > precedence (expr_to_op expr) -> "(" ^ string ^ ")"
    | _ -> string
  and binary_op symbol expr left right =
    py_expr (Some expr) left ^ symbol ^ py_expr (Some expr) right
  in py_expr None

(******************************************************)

let naturl_to_python ~annotate ~code =
  let indent depth = String.make (4 * depth) ' ' in
  let rec ast_to_python ~depth = function
    | Body body ->
      body
      |> List.map
        (function ast -> ast_to_python ~depth ast ^ "\n")
      |> String.concat "\n"
    | Expr (_, expression) ->
      indent depth ^ py_expr expression
    | Return (_, expression) ->
      let expression = py_expr expression in
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
      indent depth ^ name ^ annotation ^ " = " ^ py_expr expr
    | If (_, condition, body, else_) ->
      let body = ast_to_python ~depth:(depth + 1) body in
      let condition = py_expr condition in
      let else_ = match else_ with
        | Some body -> indent depth ^ "else:\n" ^ (ast_to_python ~depth:(depth + 1) body)
        | None -> ""
      in
      indent depth ^ "if " ^ condition ^ ":\n" ^ body ^ else_
    | Else (_, body) ->
      let body = ast_to_python ~depth:(depth + 1) body in
      indent depth ^ "else:\n" ^ body
    | For (_, var, start, end_, body) ->
      let body = ast_to_python ~depth:(depth + 1) body
      and start = py_expr start
      and end_ = py_expr end_ in
      indent depth ^ "for " ^ var ^ " in range(" ^ start ^ ", " ^ end_ ^ "):\n" ^ body
    | For_each (_, var, iterable, body) ->
      let body = ast_to_python ~depth:(depth + 1) body
      and iterable = py_expr iterable in
      indent depth ^ "for " ^ var ^ " in " ^ iterable ^ ":\n" ^ body
    | While (_, condition, body) ->
      let body = ast_to_python ~depth:(depth + 1) body
      and condition = py_expr condition in
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
