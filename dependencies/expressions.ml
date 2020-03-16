(* #load "str.cma" *)

open Utils
open Errors
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

let string_of_operator : Expr.t -> string = function
  | Plus _ -> "+"
  | Minus _ | Expr.Neg _ -> "-"
  | Times _ -> "*"
  | Div _ -> "/"
  | Div_int _ -> "div"
  | Modulus _ -> "mod"
  | Eq _ -> "="
  | Gt _ -> ">"
  | Gt_eq _ -> ">="
  | Lt _ -> "<"
  | Lt_eq _ -> "<="
  | And _ -> "et"
  | Or _ -> "ou"
  | Not _ -> "non"
  | Value v -> raise_syntax_error (Value.to_string v ^ "is no operator")

let op_priority = function
  | "" -> max_int
  | "ou" -> 1
  | "et" -> 2
  | ">" | ">=" | "<" | "<=" | "=" -> 3
  | "+" | "-" -> 4
  | "*" | "/" | "div" -> 5
  | "non" -> 6
  | op -> raise_syntax_error ("Unknown operator '" ^ op ^ "'") ;;


let make_binary_op op e1 e2 : Expr.t =
  match op with
  | "+" ->  Plus (e1, e2)
  | "-" -> Minus (e1, e2)
  | "*" -> Times (e1, e2)
  | "/" -> Div (e1, e2)
  | "div" -> Div_int (e1, e2)
  | "mod" -> Modulus (e1, e2)
  | "=" -> Eq (e1, e2)
  | ">" -> Gt (e1, e2)
  | ">=" -> Gt_eq (e1, e2)
  | "<" -> Lt (e1, e2)
  | "<=" -> Lt_eq (e1, e2)
  | "et" -> And (e1, e2)
  | "ou" -> Or (e1, e2)
  | op -> raise_syntax_error ("Unknown operator '" ^ op ^ "'")

let make_unary_op op arg : Expr.t =
  match op with
  | "-" -> Neg arg
  | "non" -> Not arg
  | op -> raise_syntax_error ("Unknown operator '" ^ op ^ "'")

(* Converts a string to an expression (bonus : returns its type)*)
let expr_of_string str =
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
         Expr.Value (Value.of_string str)
         (* If op is a binary operator: *)
       else if not (List.mem op unary_ops) then
         let left_expr = _expr_of_str left and right_expr = _expr_of_str right
         in make_binary_op op left_expr right_expr
       else
         (* It is supposed that all unary operators are prefixes *)
         make_unary_op op (_expr_of_str right)
  in _expr_of_str str ;;

let rec string_of_expr : Expr.t -> string = function
  | Plus (e1, e2) -> "(" ^ (string_of_expr e1) ^ " + " ^ (string_of_expr e2) ^ ")"
  | Minus (e1, e2) -> "(" ^ (string_of_expr e1) ^ " - " ^ (string_of_expr e2) ^ ")"
  | Times (e1, e2) -> "(" ^ (string_of_expr e1) ^ " * " ^ (string_of_expr e2) ^ ")"
  | Div (e1, e2) -> "(" ^ (string_of_expr e1) ^ " / " ^ (string_of_expr e2) ^ ")"
  | Div_int (e1, e2) -> "(" ^ (string_of_expr e1) ^ " div " ^ (string_of_expr e2) ^ ")"
  | Modulus (e1, e2) -> "(" ^ (string_of_expr e1) ^ " mod " ^ (string_of_expr e2) ^ ")"
  | Eq (e1, e2) -> "(" ^ (string_of_expr e1) ^ " = " ^ (string_of_expr e2) ^ ")"
  | Gt (e1, e2) -> "(" ^ (string_of_expr e1) ^ " > " ^ (string_of_expr e2) ^ ")"
  | Gt_eq (e1, e2) -> "(" ^ (string_of_expr e1) ^ " >= " ^ (string_of_expr e2) ^ ")"
  | Lt (e1, e2) -> "(" ^ (string_of_expr e1) ^ " < " ^ (string_of_expr e2) ^ ")"
  | Lt_eq (e1, e2) -> "(" ^ (string_of_expr e1) ^ " <= " ^ (string_of_expr e2) ^ ")"
  | And (e1, e2) -> "(" ^ (string_of_expr e1) ^ " et " ^ (string_of_expr e2) ^ ")"
  | Or (e1, e2) -> "(" ^ (string_of_expr e1) ^ " ou " ^ (string_of_expr e2) ^ ")"
  | Not e -> "(non "  ^ (string_of_expr e) ^ ")"
  | Neg e -> "(-" ^ (string_of_expr e) ^ ")"
  | Value v -> Value.to_string v
