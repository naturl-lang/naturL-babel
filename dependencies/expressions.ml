(* #load "str.cma" *)

open Errors
open Utils
open Tokenizer
open Structures

let binary_ops = [
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

let unary_ops = ["non"; "neg"]

let expr_to_op : Expr.t -> string = function
  | Plus _ -> "+"
  | Minus _ | Neg _ -> "-"
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
  | Or _ -> "or"
  | Not _ -> "non"
  | List _ -> "["
  | Call (name, _) -> name
  | Value _ -> ""

let precedence = function
  | "" -> max_int
  | "ou" -> 1
  | "et" -> 2
  | ">" | ">=" | "<" | "<=" | "=" -> 3
  | "+" | "-" -> 4
  | "*" | "/" | "div" -> 5
  | "non" | "neg" | "[" -> 6
  | _ -> 6 (* Function call *)

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
  | "neg" -> Neg arg
  | "non" -> Not arg
  | op -> raise_syntax_error ("Unknown operator '" ^ op ^ "'")

let string_of_token = function
  | Operator str | Identifier str | Litteral str -> str
  | OpenP | CloseP | OpenHook | CloseHook -> ""
  | Coma -> ","

let rec string_of_tokens = function
    [] -> ""
  | token :: t -> string_of_token token ^ string_of_tokens t

let expr_of_string str : Expr.t =
  (*Expression simplification functions: *) 
  let is_var value = 
    match value with 
      |Value.Variable _ -> true 
      |_-> false 
  in 
  let perform_op e1 e2 op expr_ = 
    match op,e1,e2 with 
      |"+", Expr.Value(Value.Int x1), Expr.Value(Value.Int x2) -> Expr.Value(Value.Int (x1+x2))
      |"+", Expr.Value(Value.Float x1), Expr.Value(Value.Float x2) -> Expr.Value(Value.Float (x1 +. x2))
      |"+", Expr.Value(Value.String x1), Expr.Value(Value.String x2) -> Expr.Value(Value.String (x1^x2))
      |"-", Expr.Value(Value.Int x1), Expr.Value(Value.Int x2) -> Expr.Value(Value.Int (x1 - x2))
      |"-", Expr.Value(Value.Float x1), Expr.Value(Value.Float x2) -> Expr.Value(Value.Float (x1 -. x2))
      |"*", Expr.Value(Value.Int x1), Expr.Value(Value.Int x2) -> Expr.Value (Value.Int (x1 *x2)) 
      |"*", Expr.Value(Value.Float x1), Expr.Value(Value.Float x2) -> Expr.Value (Value.Float (x1 *. x2)) 
      |"\\", Expr.Value(Value.Float x1), Expr.Value(Value.Float x2) -> Expr.Value (Value.Float (x1 /. x2))
      |"div", Expr.Value(Value.Int x1), Expr.Value(Value.Int x2) -> Expr.Value (Value.Int (x1 / x2))
      |"%",Expr.Value(Value.Int x1), Expr.Value(Value.Int x2) -> Expr.Value(Value.Int (x1 mod x2))
      |"=", Expr.Value v1, Expr.Value v2 when not((is_var v1) || (is_var v2)) -> Expr.Value(Value.Bool ((Value.to_string v1) = (Value.to_string v2)))
      |">", Expr.Value (Value.Int x1), Expr.Value(Value.Int x2) -> Expr.Value (Value.Bool (x1 > x2)) 
      |">", Expr.Value (Value.Float x1), Expr.Value(Value.Float x2) -> Expr.Value (Value.Bool (x1 > x2)) 
      |">=", Expr.Value (Value.Int x1), Expr.Value(Value.Int x2) -> Expr.Value (Value.Bool (x1 >=x2)) 
      |">=", Expr.Value (Value.Float x1), Expr.Value(Value.Float x2) -> Expr.Value (Value.Bool (x1 >= x2)) 
      |"<", Expr.Value (Value.Int x1), Expr.Value(Value.Int x2) -> Expr.Value (Value.Bool (x1 < x2)) 
      |"<", Expr.Value (Value.Float x1), Expr.Value(Value.Float x2) -> Expr.Value (Value.Bool (x1 < x2))
      |"<=", Expr.Value (Value.Int x1), Expr.Value(Value.Int x2) -> Expr.Value (Value.Bool (x1 <= x2)) 
      |"<=", Expr.Value (Value.Float x1), Expr.Value(Value.Float x2) -> Expr.Value (Value.Bool (x1 <= x2))  
      |"and", Expr.Value (Value.Bool x1), Expr.Value(Value.Bool x2) -> Expr.Value (Value.Bool (x1 && x2))
      |"and", Expr.Value (Value.Bool false), _ | "and", _, Expr.Value(Value.Bool false) -> Expr.Value(Value.Bool false)
      |"or", Expr.Value (Value.Bool x1), Expr.Value(Value.Bool x2) -> Expr.Value (Value.Bool (x1 || x2))
      |"or", Expr.Value (Value.Bool true), _ | "or", _ , Expr.Value(Value.Bool true) -> Expr.Value(Value.Bool true)
      |"not", Expr.Value (Value.Bool x), _ -> Expr.Value(Value.Bool (not x))
      |"neg", Expr.Value (Value.Int x), _ -> Expr.Value(Value.Int (-x))
      |"neg", Expr.Value (Value.Float x), _ -> Expr.Value (Value.Float (-1.*. x))
      |_-> expr_ (*Case containing a variable*)
  in 
  let rec _simplify expr = 
    match expr with 
      |Expr.Plus (e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "+" (Expr.Plus(e1,e2))
      |Expr.Minus (e1,e2) -> let e1, e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "-" (Expr.Minus(e1,e2))
      |Expr.Times (e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "*" (Expr.Times(e1,e2))
      |Expr.Div (e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "\\" (Expr.Div(e1,e2))
      |Expr.Div_int (e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "div" (Expr.Div_int(e1,e2))
      |Expr.Modulus (e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "%" (Expr.Modulus(e1,e2))
      |Expr.Eq(e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "=" (Expr.Eq(e1,e2))
      |Expr.Gt(e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 ">" (Expr.Gt(e1,e2))
      |Expr.Gt_eq(e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 ">=" (Expr.Gt_eq(e1,e2))
      |Expr.Lt(e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "<" (Expr.Lt(e1,e2))
      |Expr.Lt_eq(e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "<=" (Expr.Lt_eq(e1,e2))
      |Expr.And(e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "and" (Expr.And(e1,e2))
      |Expr.Or(e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "or" (Expr.Or(e1,e2))
      |Expr.Not e -> let e = _simplify e in perform_op e e "not" (Expr.Not e)
      |Expr.Neg e -> let e = _simplify e in perform_op e e "neg" (Expr.Neg e)
      |_-> expr 
  in 
  (*Core functions: *)
  let split_params list =
    let rec _split_params ?(current = []) ?(depth = 0) list =
      match list with
      | [] | _ :: [] -> if current = [] then [] else [List.rev current]
      | (OpenP | OpenHook as h) :: t -> _split_params t ~current: (h :: current) ~depth: (depth + 1)
      | (CloseP | CloseHook as h) :: t -> _split_params t ~current: (h :: current) ~depth: (depth - 1)
      | Coma :: t when depth = 0 -> (List.rev current) :: _split_params t ~depth
      | h :: t -> _split_params t ~current: (h :: current) ~depth
    in _split_params (List.tl list)
  in let rec find_op ?(current = "") ?(min_prec = max_int) ?(left = Queue.create()) ?(right = Queue.create()) ?(prec = 0) tokens =
    match tokens with
    | [] -> current, left, right
    | (OpenP | OpenHook as token) :: t -> Queue.add token right;
      find_op t ~current ~left ~right ~prec: (prec + 10) ~min_prec
    | (CloseP | CloseHook as token) :: t -> Queue.add token right;
      find_op t ~current ~left ~right ~prec: (prec - 10) ~min_prec
    | Operator op :: t when prec + precedence op < min_prec ->
      if current <> "" then Queue.add (Operator current) left; Queue.transfer right left;
      find_op t ~current: op ~min_prec: (prec + precedence op) ~left ~right ~prec
    | token :: t -> Queue.add token right;
      find_op t ~current ~left ~right ~prec ~min_prec
  in let rec expr_of_tokens tokens =
       match find_op tokens with
       | "", _, _ -> Expr.Value (Value.of_string (string_of_tokens tokens))
       | op, left, right ->
         if List.mem op binary_ops then
           let left_expr = expr_of_tokens (list_of_queue left) and right_expr = expr_of_tokens (list_of_queue right)
           in make_binary_op op left_expr right_expr
         else if List.mem op unary_ops then  (* All unary tokens are prefixes *)
           make_unary_op op (expr_of_tokens (list_of_queue right))
         else if op = "[" then begin
           let right = split_params (list_of_queue right) in
           List (List.map expr_of_tokens right) end
         else (* Function call *)
           let right = split_params (list_of_queue right) in
           Call (op, List.map expr_of_tokens right)
  in  _simplify (expr_of_tokens (tokenize str))

let string_of_expr expr =
  let rec _string_of_expr ?parent (expr : Expr.t) =
    let str, op = match expr with
      | Plus (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " + " ^ (_string_of_expr ~parent: op e2), op
      | Minus (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " - " ^ (_string_of_expr ~parent: op e2), op
      | Times (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " * " ^ (_string_of_expr ~parent: op e2), op
      | Div (e1, e2) as op-> _string_of_expr ~parent: op e1 ^ " / "^ (_string_of_expr ~parent: op e2), op
      | Div_int (e1, e2) as op-> _string_of_expr ~parent: op e1 ^ " div " ^ (_string_of_expr ~parent: op e2), op
      | Modulus (e1, e2) as op-> _string_of_expr ~parent: op e1 ^ " mod " ^ (_string_of_expr ~parent: op e2), op
      | Eq (e1, e2) as op-> _string_of_expr ~parent: op e1 ^ " = " ^ (_string_of_expr ~parent: op e2), op
      | Gt (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " > " ^ (_string_of_expr ~parent: op e2), op
      | Gt_eq (e1, e2) as op-> _string_of_expr ~parent: op e1 ^ " >= " ^ (_string_of_expr ~parent: op e2), op
      | Lt (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " < " ^ (_string_of_expr ~parent: op e2), op
      | Lt_eq (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " <= " ^ (_string_of_expr ~parent: op e2), op
      | And (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " et " ^ (_string_of_expr ~parent: op e2), op
      | Or (e1, e2) as op ->  _string_of_expr ~parent: op e1 ^ " ou " ^ (_string_of_expr ~parent: op e2), op
      | Not e as op -> "non "  ^ (_string_of_expr ~parent: op e), op
      | Neg e as op-> "-" ^ _string_of_expr ~parent: op e ^ ")", op
      | List l as op -> "[" ^ (String.concat ", " (List.map _string_of_expr l)) ^ "]", op
      | Call (name, args) as op -> name ^  "(" ^ String.concat ", " (List.map _string_of_expr args) ^ ")", op
      | Value v as op -> Value.to_string v, op
    in match parent with
    | Some parent when precedence (expr_to_op parent) > precedence (expr_to_op op) -> "(" ^ str ^ ")"
    | _ -> str
  in _string_of_expr expr
