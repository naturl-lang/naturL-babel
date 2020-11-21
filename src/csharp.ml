open Utils
open Ast
open Type
open Expressions
open Parser
open Check_semantic
open Errors

(********************* Type *********************)

let rec csharp_type: Type.t -> string = function
  | Int -> "int"
  | Float -> "float"
  | Char -> "char"
  | String -> "string"
  | Bool -> "bool"
  | List t -> csharp_type t ^ "[]"
  | Function (params, return) ->
    "Func<" ^ (params
               |> List.map csharp_type
               |> String.concat (", " ^ if params <> [] then "" else ", " ^ csharp_type return))
  | None -> "void"
  | Any -> "object"
  | Class _ -> "object"
  | Custom class_name -> class_name
  | Union _ -> raise_syntax_error "Les unions de type ne sont pas supportées en C#"

(********************* Expression *********************)

let csharp_expr =
  let rec csharp_expr (parent: Expr.t option) (expression: Expr.t) =
    let string, expr = match expression with
    | Plus (left, right) as expr -> binary_op " + " expr left right, expr
    | Minus (left, right) as expr -> binary_op " - " expr left right, expr
    | Times (left, right) as expr -> binary_op " * " expr left right, expr
    | Div (left, right) | Div_int (left, right) as expr ->
      binary_op " / " expr left right, expr
    | Modulus (left, right) as expr -> binary_op " % " expr left right, expr
    | Pow (left, right) as expr ->
      "Math.Pow(" ^ csharp_expr None left ^ ", " ^ csharp_expr None right, expr
    | Neg arg as expr -> "-" ^ csharp_expr (Some expr) arg, expr
    | Eq (left, right) as expr -> binary_op " == " expr left right, expr
    | Gt (left, right) as expr -> binary_op " > " expr left right, expr
    | Lt (left, right) as expr -> binary_op " < " expr left right, expr
    | Gt_eq (left, right) as expr -> binary_op " >= " expr left right, expr
    | Lt_eq (left, right) as expr -> binary_op " <= " expr left right, expr
    | And (left, right) as expr -> binary_op " && " expr left right, expr
    | Or (left, right) as expr -> binary_op " || " expr left right, expr
    | Not arg as expr -> "!" ^ csharp_expr (Some expr) arg, expr
    | List l as expr ->
      "{" ^ String.concat ", " (List.map (csharp_expr None) l) ^ "}", expr
    | Call (name, args) as expr ->
      String.capitalize_ascii name ^ "(" ^
      String.concat ", " (List.map (csharp_expr None) args) ^ ")", expr
    | Subscript (arg, index) as expr ->
      csharp_expr (Some expr) arg ^ "[" ^ csharp_expr None index ^ "]", expr
    | Value value as expr ->
      begin
        match value with
        | Int int -> Big_int.string_of_big_int int
        | Float float -> string_of_float float
        | Char char -> "'" ^ String.make 1 char ^ "'"
        | String string -> {|"|} ^ string ^ {|"|}
        | Bool bool -> string_of_bool bool
        | Variable name -> name
        | Instance _ -> assert false
        | None -> "void"
      end, expr
    | Access (var, attr) as expr -> csharp_expr (Some expr) var ^ "." ^ attr, expr
    in match parent with
    | Some parent when precedence (expr_to_op parent) > precedence (expr_to_op expr) -> "(" ^ string ^ ")"
    | _ -> string
  and binary_op symbol expr left right =
    csharp_expr (Some expr) left ^ symbol ^ csharp_expr (Some expr) right
  in csharp_expr None

(******************************************************)

let naturl_to_csharp ~code =
  let indent depth = String.make (4 * depth) ' ' in
  let rec ast_to_csharp ?(show_brace = true) ~depth = function
    | Body body ->
      (if show_brace then indent (depth - 1) ^ "{\n" else "") ^
      (body
       |> List.map
         (function ast -> ast_to_csharp ~depth ast)
       |> String.concat "\n") ^
      "\n" ^ (if show_brace then indent (depth - 1) ^ "}\n" else "")
    | Expr (_, expression) ->
      indent depth ^ csharp_expr expression ^ ";"
    | Return (_, expression) ->
      let expression = csharp_expr expression in
      indent depth ^ "return " ^ expression ^ ";"
    | Assign (location, name, expr) ->
      let variables = Variables.get_locale_variables location in
      let type_ = variables
                  |> StringMap.map (fun _ -> location)
                  |> type_of_expr expr
                  |> try_update_err location (fun () -> csharp_type)
      in
      indent depth ^ type_ ^ " " ^ name ^ " = " ^ csharp_expr expr ^ ";"
    | If (_, condition, body, else_) ->
      let body = ast_to_csharp ~depth:(depth + 1) body in
      let condition = csharp_expr condition in
      let else_ = match else_ with
        | Some body -> indent depth ^ "else\n" ^ (ast_to_csharp ~depth:(depth + 1) body)
        | None -> ""
      in
      indent depth ^ "if (" ^ condition ^ ")\n" ^ body ^ else_
    | Else (_, body) ->
      let body = ast_to_csharp ~depth:(depth + 1) body in
      indent depth ^ "else\n" ^ body
    | For (_, var, start, end_, body) ->
      let body = ast_to_csharp ~depth:(depth + 1) body
      and start = csharp_expr start
      and end_ = csharp_expr end_ in
      indent depth ^ "for (int " ^ var ^ " = " ^ start ^ "; " ^
      var ^ " < " ^ end_ ^ "; " ^ var ^ "++)\n" ^ body
    | For_each (location, var, iterable, body) ->
      let variables = Variables.get_locale_variables location in
      let body = ast_to_csharp ~depth:(depth + 1) body
      and iterable = csharp_expr iterable
      and iterable_type = variables
                          |> StringMap.map (fun _ -> location)
                          |> type_of_expr iterable
                          |> try_update_err location (fun () -> csharp_type)
      in
      indent depth ^ "foreach (" ^ iterable_type ^ " " ^ var ^
      " in " ^ iterable ^ ")\n" ^ body
    | While (_, condition, body) ->
      let body = ast_to_csharp ~depth:(depth + 1) body
      and condition = csharp_expr condition in
      indent depth ^ "while (" ^ condition ^ ")\n" ^ body
    | Func_definition (location, name, args, return, body) ->
      let name = String.capitalize_ascii name
      and body = ast_to_csharp ~depth:(depth + 1) body
      and types, args = List.split args in
      let args = List.map2
          (fun arg -> fun type_ -> (Type.of_string type_ |> try_update_err location (fun () -> csharp_type)) ^ " " ^ arg)
          args types
      in
      let return = Type.of_string return |> try_update_err location (fun () -> csharp_type) in
      indent depth ^ "private static " ^ return ^ " " ^ name ^
      "(" ^ (String.concat ", " args) ^ ")\n" ^ body
    | End -> "\n"
  in let ast = try_catch stderr (fun () -> parse_body code)
  in try_catch stderr (fun () -> check_semantic ast);
  let main, functions = match ast with
    | Body body ->
      let main, functions = body |> List.partition (function
          | Func_definition _ -> false
          | _ -> true)
      in Body main, Body functions
    | _ -> assert false
  in
  let main = ast_to_csharp ~depth:3 main in
  let functions = ast_to_csharp ~show_brace:false ~depth:2 functions in
  "namespace Application\n{\n" ^
  indent 1 ^ "static class Program\n" ^ indent 1 ^ "{\n" ^
  indent 2 ^ "static void Main(string[] args)\n" ^
  indent 2 ^ String.trim main ^ "\n\n" ^
  indent 2 ^ String.trim functions ^ "\n" ^
  "\n}\n"
