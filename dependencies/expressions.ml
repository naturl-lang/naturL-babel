(* #load "str.cma" *)
open Str ;;
open Structures ;;

(* TODO: Add operators of more than one char *)

let operators = [
    "+";
    "-";
    "*";
    "/";
    ">";
    "<";
    "="
  ] ;;

let op_priority = function
  | "" -> max_int
  | "+" | "-" -> 1
  | "*" | "/" -> 2
  | ">" | "<" | "=" -> 3
  | op -> failwith ("op_priority: Unknown operator: " ^ op) ;;

let make_bin_op arg1 arg2 = function
  | "+" -> Plus (arg1, arg2)
  | "-" -> Minus (arg1, arg2)
  | "*" -> Times (arg1, arg2)
  | "/" -> Divide (arg1, arg2)
  | "=" -> Equal (arg1, arg2)
  | ">" -> Greater (arg1, arg2)
  | "<" -> Lower (arg1, arg2)
  | op -> failwith ("make_bin_op: Unknown operator: " ^ op);;

let str_to_struct str =
  if string_match (regexp "^-?[0-9]+$") str 0 then
    Int (int_of_string str)
  else if string_match (regexp "^[0-9]+\\.[0-9]+$") str 0 then
    Float (float_of_string str)
  else if string_match (regexp "^\".*\"$") str 0 then
    String (String.sub str 1 (String.length str - 2))
  else if string_match (regexp "^'.+'$") str 0 then
    Char str.[1]
  else if str = "true" then
    Boolean true
  else if str = "false" then
    Boolean false
  else
    failwith ("str_to_struct: Unknown type_struct: " ^ str) ;;

let str_to_expr str =
  let rec find_op str current prev next priority =
    let length = String.length str in
    if length = 0 then
      current, prev, next
    else
      let ch = String.make 1 str.[0] in
      let next_str = String.sub str 1 (length - 1) in
      if ch = "(" then
        find_op next_str current prev (next ^ ch) (priority + 10)
      else if ch = ")" then
        find_op next_str current prev (next ^ ch) (priority - 10)
      else if List.mem ch operators && priority + op_priority ch <= op_priority current then
        find_op next_str ch (prev ^ current ^ next) "" priority
      else
        find_op next_str current prev (next ^ ch) priority
  in let rec _str_to_expr str =
       let op, prev, next = find_op str "" "" "" 0 in
       if op = "" then
         let str = String.trim str in
         let str = if str.[0] = '(' then String.sub str 1 (String.length str - 1) else str in
         let str = if str.[String.length str - 1] = ')' then String.sub str 0 (String.length str - 1) else str in
         Value (str_to_struct (String.trim str))
       else
         make_bin_op (_str_to_expr prev) (_str_to_expr next) op
     in _str_to_expr str ;;

let rec expr_to_npn = function
  | Plus (arg1, arg2) -> "+" ^ " " ^ expr_to_npn arg1 ^ " " ^ expr_to_npn arg2
  | Minus (arg1, arg2) -> "-" ^ " " ^ expr_to_npn arg1 ^ " " ^ expr_to_npn arg2
  | Times (arg1, arg2) -> "*" ^ " " ^ expr_to_npn arg1 ^ " " ^ expr_to_npn arg2
  | Divide (arg1, arg2) -> "/" ^ " " ^ expr_to_npn arg1 ^ " " ^ expr_to_npn arg2
  | Equal (arg1, arg2) -> "=" ^ " " ^ expr_to_npn arg1 ^ " " ^ expr_to_npn arg2
  | Greater (arg1, arg2) -> ">" ^ " " ^ expr_to_npn arg1 ^ " " ^ expr_to_npn arg2
  | Lower (arg1, arg2) -> "<" ^ " " ^ expr_to_npn arg1 ^ " " ^ expr_to_npn arg2
  | Value type_struct ->
     match type_struct with
     | Int i -> string_of_int i
     | Float f -> string_of_float f
     | Char c -> String.make 1 c
     | String s -> s
     | Boolean b -> if b then "true" else "false"
     | List _ | Function _ -> failwith "Unsupported type."
