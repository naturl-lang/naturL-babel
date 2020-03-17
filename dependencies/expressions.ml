(* #load "str.cma" *)

open Str
open Utils
open Errors
open Getters
open Structures

(* Binary operators *)
let operators = [
  "+";
  "-";
  "*";
  "div";
  "/";
  ">";
  ">=";
  "<";
  "<=";
  "=";
  "et";
  "ou";
  "non"
] ;;

let unary_ops = ["non"]

let string_of_operator = function
  | Plus _ -> "+"
  | Minus _ -> "-"
  | Times _ -> "*"
  | Divide _ -> "/"
  | Equal _ -> "="
  | Greater _ -> ">"
  | GreaterOrEqual _ -> ">="
  | Lower _ -> "<"
  | LowerOrEqual _ -> "<="
  | And _ -> "et"
  | Or _ -> "ou"
  | Not _ -> "non"
  | Value v -> Type.string_of_type v

let op_priority = function
  | "" -> max_int
  | "ou" -> 1
  | "et" -> 2
  | ">" | ">=" | "<" | "<=" | "=" -> 3
  | "+" | "-" -> 4
  | "*" | "/" | "div" -> 5
  | "non" -> 6
  | op -> raise_syntax_error ("Unknown operator '" ^ op ^ "'") ;;

let make_binary_op arg1 arg2 = function
  | "+" -> Plus (arg1, arg2)
  | "-" -> Minus (arg1, arg2)
  | "*" -> Times (arg1, arg2)
  | "/" | "div" -> Divide (arg1, arg2)
  | "=" -> Equal (arg1, arg2)
  | ">" -> Greater (arg1, arg2)
  | ">=" -> GreaterOrEqual (arg1, arg2)
  | "<" -> Lower (arg1, arg2)
  | "<=" -> LowerOrEqual (arg1, arg2)
  | "et" -> And (arg1, arg2)
  | "ou" -> Or (arg1, arg2)
  | op -> raise_syntax_error ("Unknown operator '" ^ op ^ "'")

let make_unary_op arg = function
  | "non" -> Not arg
  | op -> raise_syntax_error ("Unknown operator '" ^ op ^ "'")

(* Converts a string to a type_struct *)
let struct_of_str str vars =
  if string_match (regexp "^-?[0-9]+$") str 0 then
    Type.Int
  else if string_match (regexp "^[0-9]+\\.[0-9]+$") str 0 then
    Type.Float
  else if string_match (regexp "^\".*\"$") str 0 then
    Type.String
  else if string_match (regexp "^'.+'$") str 0 then
    Type.Char
  else if str = "vrai" || str = "faux" then
    Type.Boolean
  else if string_match (regexp "^[A-z_][A-z0-9_]*$") str 0 then (* str is a variable *)
    let vars = get_var_by_name str vars in
    vars.type_struct
  else
    raise_syntax_error ("Can not resolve operand '" ^ str ^ "'") ;;

(* Converts a string to an expression *)
let expr_of_str str vars =
  let rec find_op str buf current left right priority =
    let length = String.length str in
    if length = 0 then
      let right = if String.length buf <> 0 && not (List.mem buf operators) then
          right ^ buf
        else
          right
      in current, left, right
    else
      let ch = String.make 1 str.[0] in
      let is_new_buf_pref = is_list_prefix operators (buf ^ ch)
      and next_str = String.sub str 1 (length - 1)
      and priority = if ch = "(" then priority + 10
        else if ch = ")" then priority - 10
        else priority in
      (* If the previous buffer was an operator and the current one is no more *)
      (* Compare the priority of the new operator with the priority of the current one *)
      (* If it is lower, change the current operator and clear the buffer *)
      if String.length buf <> 0 && not is_new_buf_pref && List.mem buf operators
         && priority + op_priority buf < op_priority current then
        find_op next_str "" buf (left ^ current ^ right) ch priority
        (* If the new buffer is a prefix of an operator, continue feeding it *)
      else if is_new_buf_pref then
        find_op next_str (buf ^ ch) current left right priority
        (* If the buffer was empty, just continue reading *)
      else if String.length buf = 0 then
        find_op next_str "" current left (right ^ ch) priority
        (* Else add it to the right string *)
      else
        find_op next_str "" current left (right ^ buf ^ ch) priority
  in let rec _expr_of_str str =
       let op, left, right = find_op str "" "" "" "" 0 in
       if op = "" then
         let str = String.trim str in
         let str = if str.[0] = '(' then String.sub str 1 (String.length str - 1) else str in
         let str = if str.[String.length str - 1] = ')' then String.sub str 0 (String.length str - 1) else str in
         Value (struct_of_str (String.trim str) vars)
         (* If op is a binary operator: *)
       else if not (List.mem op unary_ops) then
         make_binary_op (_expr_of_str left) (_expr_of_str right) op
       else
         (* It is supposed that all unary operators are prefixes *)
         make_unary_op (_expr_of_str right) op
  in _expr_of_str str ;;

(* Converts an expression to its string representation as a string. Used mostly for debugging. *)
let tree_of_expr expr =
  let rec _tree_of_expr expr d = match expr with
    | Plus (arg1, arg2) -> d ^ "|- Plus" ^ "\n" ^
                           (_tree_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                           (_tree_of_expr arg2 (d ^ "  "))
    | Minus (arg1, arg2) -> d ^ "|- Minus" ^ "\n" ^
                            (_tree_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                            (_tree_of_expr arg2 (d ^ "  "))
    | Times (arg1, arg2) -> d ^ "|- Times" ^ "\n" ^
                            (_tree_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                            (_tree_of_expr arg2 (d ^ "  "))
    | Divide (arg1, arg2) -> d ^ "|- Divide" ^ "\n" ^
                             (_tree_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                             (_tree_of_expr arg2 (d ^ "  "))
    | Equal (arg1, arg2) -> d ^ "|- Equal" ^ "\n" ^
                            (_tree_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                            (_tree_of_expr arg2 (d ^ "  "))
    | Greater (arg1, arg2) -> d ^ "|- Greater" ^ "\n" ^
                              (_tree_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                              (_tree_of_expr arg2 (d ^ "  "))
    | GreaterOrEqual (arg1, arg2) -> d ^ "|- GreaterOrEqual" ^ "\n" ^
                                     (_tree_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                                     (_tree_of_expr arg2 (d ^ "  "))
    | Lower (arg1, arg2) -> d ^ "|- Lower" ^ "\n" ^
                            (_tree_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                            (_tree_of_expr arg2 (d ^ "  "))
    | LowerOrEqual (arg1, arg2) -> d ^ "|- LowerOrEqual" ^ "\n" ^
                                   (_tree_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                                   (_tree_of_expr arg2 (d ^ "  "))
    | And (arg1, arg2) -> d ^ "|- And" ^ "\n" ^
                          (_tree_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                          (_tree_of_expr arg2 (d ^ "  "))
    | Or (arg1, arg2) -> d ^ "|- Or" ^ "\n" ^
                         (_tree_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                         (_tree_of_expr arg2 (d ^ "  "))
    | Not arg -> d ^ "|- Minus" ^ "\n" ^ (_tree_of_expr arg (d ^ "  "))
    | Value type_struct -> d ^ "|- " ^ Type.string_of_type type_struct
  in _tree_of_expr expr ""

(* Checks if the types of an expression are valid *)
let expr_type expr vars =
  let rec _check_binary_expr ?return_type op1 op2 is_accepted operator =
    let type1 = _check_expr op1
    and type2 = _check_expr op2
    in if not (is_accepted type1) then
      raise_type_error ("Unsupported type '" ^ (Type.string_of_type type1) ^ "' for operator " ^ (string_of_operator operator))
    else if type1 <> type2 then
      raise_unexpected_type_error (Type.string_of_type type1) (Type.string_of_type type2)
    else match return_type with
      | Some t -> t
      | None -> type1
  (* Returns a tuple (is_valid, type_struct) and checks the subtree(s). *)
  and _check_expr expr =
    match expr with
    | Value type_struct -> type_struct
    | Plus (op1, op2) as operator -> _check_binary_expr op1 op2 (function Type.Int | Type.Float | Type.String -> true | _ -> false) operator
    | Minus (op1, op2) | Times (op1, op2) | Divide (op1, op2) as operator -> _check_binary_expr op1 op2 (function Type.Int | Type.Float -> true | _ -> false) operator
    | Equal (op1, op2) as operator -> _check_binary_expr op1 op2 ~return_type: Type.Boolean
                            (function Type.Int | Type.Float | Type.String | Type.Char | Type.Boolean | Type.List _ -> true | _ -> false) operator
    | Greater (op1, op2) | GreaterOrEqual (op1, op2) | Lower (op1, op2) | LowerOrEqual (op1, op2) as operator ->
      _check_binary_expr op1 op2 (function Type.Int | Type.Float | Type.String | Type.Char -> true | _ -> false) operator ~return_type: Type.Boolean
    | And (op1, op2) | Or (op1, op2) as operator -> _check_binary_expr op1 op2 (fun t -> t = Type.Boolean) operator
    | Not op as operator -> _check_binary_expr op (Value Type.Boolean) (fun t -> t = Type.Boolean) operator
  in _check_expr (expr_of_str expr vars)
