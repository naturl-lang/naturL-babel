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
let struct_of_str str =
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
    Type.Int (* TODO: Make the type check *)
  else
    failwith ("str_to_struct: Unknown type: " ^ str) ;;

(* Converts a string to an expression *)
let expr_of_str str =
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
  in let rec _expr_of_str str =
       let op, left, right = find_op str "" "" "" "" 0 in
       if op = "" then
         let str = String.trim str in
         let str = if str.[0] = '(' then String.sub str 1 (String.length str - 1) else str in
         let str = if str.[String.length str - 1] = ')' then String.sub str 0 (String.length str - 1) else str in
         Value (struct_of_str (String.trim str))
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
    | Value _ -> d ^ "|- Value"
  in _tree_of_expr expr ""

(* Checks if the types of an expression are valid *)
let check_expr expr =
  let rec _check_binary_expr op1 op2 accepted_types =
    let is_valid1, type1 = _check_expr op1
    and is_valid2, type2 = _check_expr op2
    in let is_valid = is_valid1 && is_valid2 && type1 = type2 && List.mem type1 accepted_types
    in is_valid, if is_valid then type1 else Type.None
  (* Returns a tuple (is_valid, type_struct) and checks the subtree(s). *)
  and _check_expr expr =
    match expr with
    | Value type_struct -> true, type_struct
    | Plus (op1, op2) -> _check_binary_expr op1 op2 [Type.Int; Type.Float; Type.String]
    | Minus (op1, op2) | Times (op1, op2) | Divide (op1, op2) -> _check_binary_expr op1 op2 [Type.Int; Type.Float]
    | Equal (op1, op2) | Greater (op1, op2) | GreaterOrEqual (op1, op2)
    | Lower (op1, op2) | LowerOrEqual (op1, op2)
    | And (op1, op2) | Or (op1, op2) -> _check_binary_expr op1 op2 [Type.Boolean]
    | Not op -> _check_binary_expr op (Value Type.Boolean) [Type.Boolean]
  in _check_expr expr
