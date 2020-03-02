(* #load "str.cma" *)
open Str
open Structures

(* Binary operators *)
let operators = [
  "+";
  "-";
  "*";
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

let rec is_prefix word pref =
  let w_len = String.length word
  and p_len = String.length pref in
  p_len = 0 || w_len <> 0 && word.[0] = pref.[0]
               && is_prefix (String.sub word 1 (w_len - 1)) (String.sub pref 1 (p_len - 1))

(* Returns true if one word in the list starts with prefix *)
let rec is_list_prefix list pref = match list with
  | w :: t -> is_prefix w pref || is_list_prefix t pref
  | _ -> false ;;

let op_priority = function
  | "" -> max_int
  | "ou" -> 1
  | "et" -> 2
  | ">" | ">=" | "<" | "<=" | "=" -> 3
  | "+" | "-" -> 4
  | "*" | "/" -> 5
  | "non" -> 6
  | op -> failwith ("op_priority: Unknown operator: " ^ op) ;;

let make_binary_op arg1 arg2 = function
  | "+" -> Plus (arg1, arg2)
  | "-" -> Minus (arg1, arg2)
  | "*" -> Times (arg1, arg2)
  | "/" -> Divide (arg1, arg2)
  | "=" -> Equal (arg1, arg2)
  | ">" -> Greater (arg1, arg2)
  | ">=" -> GreaterOrEqual (arg1, arg2)
  | "<" -> Lower (arg1, arg2)
  | "<=" -> LowerOrEqual (arg1, arg2)
  | "et" -> And (arg1, arg2)
  | "ou" -> Or (arg1, arg2)
  | op -> failwith ("make_binary_op: Unknown binary operator: " ^ op)

let make_unary_op arg = function
  | "non" -> Not arg
  | op -> failwith ("make_unary_op: Unknown unary operator: " ^ op)

(* Converts a string to a type_struct *)
let str_to_struct str =
  if string_match (regexp "^-?[0-9]+$") str 0 then
    Int (int_of_string str)
  else if string_match (regexp "^[0-9]+\\.[0-9]+$") str 0 then
    Float (float_of_string str)
  else if string_match (regexp "^\".*\"$") str 0 then
    String (String.sub str 1 (String.length str - 2))
  else if string_match (regexp "^'.+'$") str 0 then
    Char str.[1]
  else if str = "vrai" then
    Boolean true
  else if str = "faux" then
    Boolean false
  else if string_match (regexp "^[A-z_][A-z0-9_]*$") str 0 then
    Variable {name = str; type_struct = Type.Int} (* TODO: Make the type check *)
  else
    failwith ("str_to_struct: Unknown type_struct: " ^ str) ;;

let str_to_expr str =
  let rec find_op str buf current left right priority =
    let length = String.length str in
    if length = 0 then
      current, left, right
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
      if String.length buf <> 0 && not is_new_buf_pref
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
  in let rec _str_to_expr str =
       let op, left, right = find_op str "" "" "" "" 0 in
       if op = "" then
         let str = String.trim str in
         let str = if str.[0] = '(' then String.sub str 1 (String.length str - 1) else str in
         let str = if str.[String.length str - 1] = ')' then String.sub str 0 (String.length str - 1) else str in
         Value (str_to_struct (String.trim str))
         (* If op is a binary operator: *)
       else if not (List.mem op unary_ops) then
         make_binary_op (_str_to_expr left) (_str_to_expr right) op
       else
         (* It is supposed that all unary operators are prefixes *)
         make_unary_op (_str_to_expr right) op
  in _str_to_expr str ;;

let string_of_expr expr =
  let rec string_of_expr expr d = match expr with
    | Plus (arg1, arg2) -> d ^ "|- Plus" ^ "\n" ^
                           (string_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                           (string_of_expr arg2 (d ^ "  "))
    | Minus (arg1, arg2) -> d ^ "|- Minus" ^ "\n" ^
                            (string_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                            (string_of_expr arg2 (d ^ "  "))
    | Times (arg1, arg2) -> d ^ "|- Times" ^ "\n" ^
                            (string_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                            (string_of_expr arg2 (d ^ "  "))
    | Divide (arg1, arg2) -> d ^ "|- Divide" ^ "\n" ^
                             (string_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                             (string_of_expr arg2 (d ^ "  "))
    | Equal (arg1, arg2) -> d ^ "|- Equal" ^ "\n" ^
                            (string_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                            (string_of_expr arg2 (d ^ "  "))
    | Greater (arg1, arg2) -> d ^ "|- Greater" ^ "\n" ^
                              (string_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                              (string_of_expr arg2 (d ^ "  "))
    | GreaterOrEqual (arg1, arg2) -> d ^ "|- GreaterOrEqual" ^ "\n" ^
                                     (string_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                                     (string_of_expr arg2 (d ^ "  "))
    | Lower (arg1, arg2) -> d ^ "|- Lower" ^ "\n" ^
                            (string_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                            (string_of_expr arg2 (d ^ "  "))
    | LowerOrEqual (arg1, arg2) -> d ^ "|- LowerOrEqual" ^ "\n" ^
                                   (string_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                                   (string_of_expr arg2 (d ^ "  "))
    | And (arg1, arg2) -> d ^ "|- And" ^ "\n" ^
                          (string_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                          (string_of_expr arg2 (d ^ "  "))
    | Or (arg1, arg2) -> d ^ "|- Or" ^ "\n" ^
                         (string_of_expr arg1 (d ^ "  ")) ^ "\n" ^
                         (string_of_expr arg2 (d ^ "  "))
    | Not arg -> d ^ "|- Minus" ^ "\n" ^ (string_of_expr arg (d ^ "  "))
    | Value _ -> d ^ "|- Value"
  in string_of_expr expr ""
