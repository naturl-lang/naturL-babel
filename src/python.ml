open Utils
open Ast
open Type
open Expressions
open Parser
open Check_semantic
open Errors

(********************* Type *********************)

let rec py_type: Type.t -> string = function
  | Int -> "int"
  | Float -> "float"
  | Char -> "str"
  | String -> "str"
  | Bool -> "bool"
  | List t ->
    Imports.add_import "typing" (Some "List");
    "List[" ^ py_type t ^ "]"
  | Function (args, return) ->
    Imports.add_import "typing" (Some "Callable");
    "Callable[[" ^ (args |> List.map py_type |> String.concat ", ") ^
    "], "^ py_type return ^ "]"
  | None -> "None"
  | Any -> Imports.add_import "typing" (Some "Any"); "Any"
  | Class _ ->
    Imports.add_import "typing" (Some "Type"); "Type"
  | Custom class_name -> class_name
  | Union types ->
    Imports.add_import "typing" (Some "Union");
    "Union[" ^ (types |> List.map py_type |> String.concat ", ") ^ "]"

(********************* Expression *********************)

let py_expr variables =
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
      let args = List.map (py_expr None) args in
      (* If the function is defined by the user *)
      if StringMap.mem name variables then
        name ^ "(" ^ String.concat ", " args ^ ")", expr
      else
        let builtin = StringMap.find name Builtins.functions in
        builtin.translator args, expr
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

let naturl_to_python ?(raise_exception = false) ~annotate ~code =
  let indent depth = String.make (4 * depth) ' ' in
  let find_declare_location variable (variables: Structures.Location.t StringMap.t) =
    let open Structures.Location in
    let rec find_declare_location = function
      | [], location -> location
      | (name, loc) :: t, location when name = variable ->
        let location = match location with
          | Some location -> if loc.line < location.line then loc else location
          | None -> loc
        in find_declare_location (t, Some location)
      | _ :: t, location -> find_declare_location (t, location)
    in find_declare_location (StringMap.bindings variables, None) |> Option.get
  in
  let rec ast_to_python ~depth ast =
    match ast with
    | Body [] -> indent depth ^ "pass"
    | Body body ->
      let _, blocks = body
      |> List.fold_left_map
        (fun parent -> fun ast ->
           let leading_space, trailing_space = match ast with
             | Func_definition _ ->
               begin
                 match parent with
                 | Func_definition _ -> ""
                 | _ -> "\n\n"
               end, "\n\n"
             | _ ->
               begin
                 match parent with
                 | While _ | For _ | For_each _ -> "\n", ""
                 | _ -> "", ""
               end
           in ast,
              leading_space ^ ast_to_python ~depth ast ^ trailing_space)
        End
      in String.concat "\n" blocks
    | Expr (location, expression) ->
      let variables = Variables.get_locale_variables location.line in
      indent depth ^ py_expr variables expression
    | Return (location, expression) ->
      let variables = Variables.get_locale_variables location.line in
      let expression = py_expr variables expression in
      indent depth ^ "return " ^ expression
    | Assign (location, name, expr) ->
      let variables = Variables.get_locale_variables location.line in
      let annotation = if not annotate || find_declare_location name variables <> location then ""
        else ": " ^ (variables |> type_of_expr expr |> py_type)
      in
      indent depth ^ name ^ annotation ^ " = " ^ py_expr variables expr
    | If (location, condition, body, else_) ->
      let variables = Variables.get_locale_variables location.line in
      let body = ast_to_python ~depth:(depth + 1) body in
      let condition = py_expr variables condition in
      let else_ = match else_ with
        | Some body -> "\n" ^ indent depth ^ "else:\n" ^ ast_to_python ~depth:(depth + 1) body
        | None -> ""
      in
      indent depth ^ "if " ^ condition ^ ":\n" ^ body ^ else_
    | Else (_, body) ->
      let body = ast_to_python ~depth:(depth + 1) body in
      indent depth ^ "else:\n" ^ body
    | For (location, var, start, end_, body) ->
      let variables = Variables.get_locale_variables location.line in
      let body = ast_to_python ~depth:(depth + 1) body
      and start = py_expr variables start
      and end_ = py_expr variables end_ in
      indent depth ^ "for " ^ var ^ " in range(" ^ start ^ ", " ^ end_ ^ "):\n" ^ body
    | For_each (location, var, iterable, body) ->
      let variables = Variables.get_locale_variables location.line in
      let body = ast_to_python ~depth:(depth + 1) body
      and iterable = py_expr variables iterable in
      indent depth ^ "for " ^ var ^ " in " ^ iterable ^ ":\n" ^ body
    | While (location, condition, body) ->
      let variables = Variables.get_locale_variables location.line in
      let body = ast_to_python ~depth:(depth + 1) body
      and condition = py_expr variables condition in
      indent depth ^ "while " ^ condition ^ ":\n" ^ body
    | Func_definition (location, name, args, return, body) ->
      let variables = Variables.get_locale_variables location.line in
      (* If the function is already defined above, add a warning *)
      if StringMap.mem name variables then
        Warnings.add_warning ("Cette redéfinition de la fonction '" ^
                              name ^ "' écrase la définition précédente") 0 ;
      let body = ast_to_python ~depth:(depth + 1) body
      and types, args = List.split args in
      let args = List.map2 (fun arg -> fun type_ ->
          let annotation = if annotate then
              ": " ^ (Type.of_string type_ |> py_type)
            else ""
          in arg ^ annotation) args types
      in
      let return = if annotate then
          " -> " ^ (Type.of_string return |> py_type)
        else ""
      in
      indent depth ^ "def " ^ name ^
      "(" ^ (String.concat ", " args) ^ ")" ^ return ^ ":\n" ^ body
    | End -> "\n"
  in let ast = try_add_error (fun () -> parse_body code) ~default:End
  in if ast <> End then try_add_error (fun () -> check_semantic ast) ~default:();
  if raise_exception then
    begin
      try_print_errors ();
      Warnings.print_warnings ~severity:0;
    end;
  if get_errors () = [] && ast <> Body [] then
    String.trim (Imports.get_imports () ^
                 "\n\n" ^
                 ast_to_python ~depth:0 ast)
    ^ "\n"
  else
    ""
