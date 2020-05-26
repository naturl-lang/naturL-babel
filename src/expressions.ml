open Errors
open Utils
open Tokenizer
open Structures
open Builtins
open Internationalisation.Translation

open (struct
  let binary_ops = [
    "+";
    "-";
    "*";
    "fois";
    "div";
    "mod";
    "^";
    "/";
    ">";
    ">=";
    "<";
    "<=";
    "=";
    "et";
    "ou";
    "get["
  ] ;;

  let unary_ops = ["non"; "neg"]

  let expr_to_op : Expr.t -> string = function
    | Plus _ -> "+"
    | Minus _ | Neg _ -> "-"
    | Times _ -> "*"
    | Div _ -> "/"
    | Div_int _ -> "div"
    | Modulus _ -> "mod"
    | Pow _ -> "^"
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
    | Subscript _ -> "get["
    | Value _ -> ""

  let precedence = function
    | "" -> max_int
    | "ou" -> 1
    | "et" -> 2
    | ">" | ">=" | "<" | "<=" | "=" -> 3
    | "+" | "-" -> 4
    | "*" | "fois" | "/" | "div" | "neg" -> 5
    | "non" | "get[" -> 6
    | "^" | "[" -> 7
    | _ -> 7 (* Function call *)

  let make_binary_op op e1 e2 : Expr.t =
    match op with
    | "+" ->  Plus (e1, e2)
    | "-" -> Minus (e1, e2)
    | "*" | "fois" -> Times (e1, e2)
    | "/" -> Div (e1, e2)
    | "div" -> Div_int (e1, e2)
    | "mod" -> Modulus (e1, e2)
    | "^" -> Pow (e1, e2)
    | "=" -> Eq (e1, e2)
    | ">" -> Gt (e1, e2)
    | ">=" -> Gt_eq (e1, e2)
    | "<" -> Lt (e1, e2)
    | "<=" -> Lt_eq (e1, e2)
    | "et" -> And (e1, e2)
    | "ou" -> Or (e1, e2)
    | "get[" -> Subscript (e1, e2)
    | op -> raise_syntax_error ((get_string UnknownOperator) ^ op ^ "'")

  let make_unary_op op arg : Expr.t =
    match op with
    | "neg" -> Neg arg
    | "non" -> Not arg
    | op -> raise_syntax_error ("Unknown operator '" ^ op ^ "'")

  let string_of_token = function
    | Operator str | Identifier str | Litteral str -> str ^ " "
    | OpenP | CloseP | OpenHook | CloseHook -> ""
    | Coma -> ", "

  let rec string_of_tokens = function
      [] -> ""
    | token :: t -> string_of_token token ^ string_of_tokens t


  let is_type_accepted t (op: Expr.t) =
    match op with
      Plus _ -> List.exists (fun expected -> Type.is_compatible expected t) [`Int; `Float; `String]
    | Minus _ | Times _ | Neg _ | Pow _ -> List.exists (fun expected -> Type.is_compatible expected t) [`Int; `Float]
    | Div_int _ | Modulus _ -> Type.is_compatible `Int t
    | Div _ -> Type.is_compatible `Float t
    | Eq _ | Gt _ | Gt_eq _ | Lt _ | Lt_eq _ -> true
    | And _ | Or _ | Not _ -> Type.is_compatible `Bool t
    | List _ | Call _ | Subscript _ | Value _ -> assert false

  let rec is_list_uniform vars = function
    | h1 :: h2 :: t -> Type.is_compatible h1 h2 && is_list_uniform vars (h2 :: t)
    | _ -> true

end)


(*********** Exported function ************)


let expr_of_string context str : Expr.t =
  (*Expression simplification functions: *)
  let is_var value =
    match value with
    | Value.Variable _ -> true
    |_-> false
  in
  let perform_op (e1: Expr.t) (e2: Expr.t) op expr_ : Expr.t =
    match op, e1, e2 with
    | "+", Value (Int x1), Value (Int x2) -> Value (Int (x1 ++ x2))
    | "+", Value (Float x1), Value (Float x2) -> Value (Float (x1 +. x2))
    | "+", Value (String x1), Value (String x2) -> Value (String (x1 ^ x2))
    | "+", Value (Int x1), Value (Float x2) -> Value(Float ((Big_int_Z.float_of_big_int x1) +. x2))
    | "+", Value (Float x1), Value (Int x2) -> Value(Float ((Big_int_Z.float_of_big_int x2) +. x1))
    | "+", Value (Char x1), Value (Char x2) -> Value (String ((Char.escaped x1) ^ (Char.escaped x2)))
    | "+", Value (String x1), Value (Char x2) -> Value (String (x1 ^ (Char.escaped x2)))
    | "+", Value (Char x1), Value (String x2) -> Value (String ((Char.escaped x1) ^ x2))
    | "-", Value (Int x1), Value (Int x2) -> Value(Int (x1 -- x2))
    | "-", Value (Float x1), Value (Float x2) -> Value (Float (x1 -. x2))
    | "-", Value (Int x1), Value (Float x2) -> Value(Float ((Big_int_Z.float_of_big_int x1) -. x2))
    | "-", Value (Float x1), Value (Int x2) -> Value(Float ((Big_int_Z.float_of_big_int x2) -. x1))
    | ("*" | "fois"), Value (Int x1), Value (Int x2) -> Value (Int (x1 ** x2))
    | ("*" | "fois"), Value (Float x1), Value (Float x2) -> Value (Float (x1 *. x2))
    | ("*" | "fois"), Value (Int x1), Value (Float x2) -> Value(Float ((Big_int_Z.float_of_big_int x1 ) *. x2))
    | ("*" | "fois"), Value (Float x1), Value (Int x2) -> Value(Float ((Big_int_Z.float_of_big_int x2 ) *. x1))
    | "\\", Value (Float x1), Value (Float x2) -> Value (Float (x1 /. x2))
    | "\\", Value (Int x1), Value (Float x2) -> Value(Float ((Big_int_Z.float_of_big_int x1) /. x2))
    | "\\", Value (Float x1), Value (Int x2) -> Value(Float (x1 /. (Big_int_Z.float_of_big_int x2)))
    | "div", Value (Int x1), Value (Int x2) -> Value (Int (x1 // x2))
    | "%", Value (Int x1), Value (Int x2) -> Value (Int (x1 % x2))
    | "=", Value v1, Value v2 when not (is_var v1 || is_var v2) -> Value (Bool Value.(to_string v1 = to_string v2))
    | ">", Value (Int x1), Value (Int x2) -> Value (Bool (x1 > x2))
    | ">", Value (Float x1), Value (Float x2) -> Value (Bool (x1 > x2))
    | ">=", Value (Int x1), Value (Int x2) -> Value (Bool (x1 >= x2))
    | ">=", Value (Float x1), Value (Float x2) -> Value (Bool (x1 >= x2))
    | "<", Value (Int x1), Value (Int x2) -> Value (Bool (x1 < x2))
    | "<", Value (Float x1), Value (Float x2) -> Value (Bool (x1 < x2))
    | "<=", Value (Int x1), Value (Int x2) -> Value (Bool (x1 <= x2))
    | "<=", Value (Float x1), Value (Float x2) -> Value (Bool (x1 <= x2))
    | "and", Value (Bool x1), Value (Bool x2) -> Value (Bool (x1 && x2))
    | "and", Value (Bool false), _ | "and", _, Value (Bool false) -> Value (Bool false)
    | "and", Value(Bool true), x -> x
    | "and", x, Value(Bool true) -> x
    | "or", Value (Bool x1), Value (Bool x2) -> Value (Bool (x1 || x2))
    | "or", Value (Bool true), _ | "or", _ , Value (Bool true) -> Value (Bool true)
    | "or", Value (Bool false), x -> x
    | "or", x, Value (Bool false) -> x
    | "not", Value (Bool x), _ -> Value (Bool (not x))
    | "neg", Value (Int x), _ -> Value (Int (~--x))
    | "neg", Value (Float x), _ -> Value (Float (-1.*. x))
    | "get[", List l, Value (Int i) when i <= to_big (List.length l) -> List.nth l (of_big i)
    | _-> expr_
  in
  let rec _simplify (expr: Expr.t) =
    match expr with
    | Plus (e1, e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "+" (Plus (e1, e2))
    | Minus (e1, e2) -> let e1, e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "-" (Minus (e1, e2))
    | Times (e1, e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "*" (Times (e1, e2))
    | Div (e1, e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "\\" (Div (e1, e2))
    | Div_int (e1, e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "div" (Div_int (e1, e2))
    | Modulus (e1, e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "%" (Modulus (e1, e2))
    | Eq (e1, e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "=" (Eq (e1, e2))
    | Gt (e1, e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 ">" (Gt (e1, e2))
    | Gt_eq (e1, e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 ">=" (Gt_eq (e1, e2))
    | Lt (e1, e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "<" (Lt (e1, e2))
    | Lt_eq (e1, e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "<=" (Lt_eq (e1, e2))
    | And(e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "and" (And (e1, e2))
    | Or(e1,e2) -> let e1,e2 = (_simplify e1, _simplify e2) in perform_op e1 e2 "or" (Or (e1, e2))
    | Not e -> let e = _simplify e in perform_op e e "not" (Not e)
    | Neg e -> let e = _simplify e in perform_op e e "neg" (Neg e)
    | _-> expr
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
           let right = split_params (list_of_queue right) |> List.map expr_of_tokens in
           if Queue.is_empty left then
             Call (op, right)
           else if Queue.length left = 1 && Queue.peek left = Identifier "instance" then
             Call ("instance " ^ op, right)
           else
             raise_syntax_error (get_string InvalidExpression)
  in _simplify (expr_of_tokens (tokenize str context.vars))


(* Returns the type of an expression *)
let rec type_of_expr context : Expr.t -> Type.t = function
  | Plus (l, r) | Minus (l, r) | Times (l, r) | Div (l, r) | Div_int (l, r) | Modulus (l, r) | Pow (l, r) | And (l, r) | Or (l, r) as e ->
    let l_type = type_of_expr context l and r_type = type_of_expr context r in
    if Type.is_compatible l_type r_type && is_type_accepted l_type e then l_type
    else if Type.is_compatible r_type l_type && is_type_accepted r_type e then r_type
    else
      raise_type_error ((get_string InvalidOperation) ^ (Type.to_string l_type) ^ (get_string AndType) ^ (Type.to_string r_type) ^ "'")
  | Not arg | Neg arg as e -> let arg_type = type_of_expr context arg in if is_type_accepted arg_type e then arg_type else
      raise_type_error ((get_string InvalidOperation) ^ (Type.to_string arg_type) ^ "'")
  | Eq (l, r) | Gt (l, r) | Gt_eq (l, r) | Lt (l, r) | Lt_eq (l, r) -> let l_type = type_of_expr context l and r_type = type_of_expr context r in
    if let open Type in is_compatible l_type r_type || is_compatible r_type l_type then
      `Bool
    else
      raise_type_error ((get_string CannotCompare) ^ (Type.to_string l_type) ^ (get_string AndType) ^ (Type.to_string r_type))
  | List [] -> `List `Any
  | List (h :: t) -> if is_list_uniform context.vars (List.map (type_of_expr context) (h :: t)) then `List (type_of_expr context h)
    else raise_type_error ("All elements of a list must have the same type") (*TODO Fix translation*)
  | Call (name, params) -> let params_types = List.map (type_of_expr context) params in
    (* If the function is a method, replace 'instance func' by 'self.func' *)
    let name = match String.split_on_char ' ' name with
        "instance" :: name :: [] -> "self." ^ name
      | name :: [] -> name
      | _ -> assert false in
    let s = match StringMap.find_opt name context.vars with
      | Some `Class (attr_meths, _) -> Some (StringMap.find "nouveau" attr_meths)   (* Constructor *)
      | s -> s in
    (try (match s with
         | Some (`Function (p, return)) -> let f = Option.get s in
           if List.length p = List.length params && List.for_all2 Type.is_compatible p params_types then  (* Despite the fact that it is not safe, function calls are covariant in argument types *)
             return
           else
             raise_unexpected_type_error_with_name name (Type.to_string f) (Type.to_string (`Function (params_types, return)))
         | Some _ as s -> let t = Option.get s in
           raise_type_error ((get_string VariablesOfType) ^ (Type.to_string t) ^ (get_string NotCallable))
         | None -> (match Value.get_unknown_variable context name (fun () -> raise Not_found) with
             | `Function (p, return) as f ->
               if List.length p = List.length params && List.for_all2 Type.is_compatible params_types p then
                 return
               else
                 raise_unexpected_type_error_with_name name (Type.to_string f) (Type.to_string (`Function (params_types, return)))
             | `Class (attr_meths, _) -> StringMap.find "nouveau" attr_meths  (* Constructor *)
             | t -> raise_type_error ((get_string VariablesOfType) ^ (Type.to_string t) ^ (get_string NotCallable))))
     with Not_found -> (try
                          let builtin = StringMap.find name Builtins.functions in
                          builtin.import ();
                          let return = builtin.typer params_types in return
                        with Not_found -> raise_name_error ((get_string UnknownFunction) ^ name ^ "'")))
  | Subscript (l, i) -> if type_of_expr context i = `Int then match type_of_expr context l with
      | `List t -> t
      | t -> raise_type_error ((get_string TheType) ^ (Type.to_string t) ^ (get_string NotSubscriptable))
    else raise_type_error (get_string ListIndicesIntegers)
  | Value v -> Value.get_type context v


(* Converts an expression to a string with parenthesis placed correctly *)
let string_of_expr context expr =
  let rec _string_of_expr ?parent (expr : Expr.t) =
    let str, op = match expr with
      | Plus (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " + " ^ (_string_of_expr ~parent: op e2), op
      | Minus (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " - " ^ (_string_of_expr ~parent: op e2), op
      | Times (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " * " ^ (_string_of_expr ~parent: op e2), op
      | Div (e1, e2) as op-> _string_of_expr ~parent: op e1 ^ " / "^ (_string_of_expr ~parent: op e2), op
      | Div_int (e1, e2) as op-> _string_of_expr ~parent: op e1 ^ " // " ^ (_string_of_expr ~parent: op e2), op
      | Modulus (e1, e2) as op-> _string_of_expr ~parent: op e1 ^ " % " ^ (_string_of_expr ~parent: op e2), op
      | Pow (e1, e2) as op-> _string_of_expr ~parent: op e1 ^ " ** " ^ (_string_of_expr ~parent: op e2), op
      | Eq (e1, e2) as op-> _string_of_expr ~parent: op e1 ^ " == " ^ (_string_of_expr ~parent: op e2), op
      | Gt (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " > " ^ (_string_of_expr ~parent: op e2), op
      | Gt_eq (e1, e2) as op-> _string_of_expr ~parent: op e1 ^ " >= " ^ (_string_of_expr ~parent: op e2), op
      | Lt (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " < " ^ (_string_of_expr ~parent: op e2), op
      | Lt_eq (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " <= " ^ (_string_of_expr ~parent: op e2), op
      | And (e1, e2) as op -> _string_of_expr ~parent: op e1 ^ " and " ^ (_string_of_expr ~parent: op e2), op
      | Or (e1, e2) as op ->  _string_of_expr ~parent: op e1 ^ " or " ^ (_string_of_expr ~parent: op e2), op
      | Not e as op -> "not "  ^ _string_of_expr ~parent: op e, op
      | Neg e as op-> "-" ^ _string_of_expr ~parent: op e, op
      | List l as op -> "[" ^ (String.concat ", " (List.map _string_of_expr l)) ^ "]", op
      | Call (name, args) as op ->
        (* If the function is a method, replace 'instance func' by 'self.func' *)
        let name = (match String.split_on_char ' ' name with
              "instance" :: name :: [] -> "self." ^ name
            | name :: [] -> name
            | _ -> assert false) in
        let name = if Str.string_match (Str.regexp "^\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\.\\(.+\\)$") name 0 then
            let var_name = Str.matched_group 1 name in
            let meth_name = Str.matched_group 2 name in
            if meth_name = "nouveau" then
              Type.to_string (context.vars |> StringMap.find var_name)
            else
              name
          else
            name in
        let translated_args = List.map _string_of_expr args in
        (try let builtin = StringMap.find name Builtins.functions in
           builtin.translator translated_args, op
         with Not_found -> name ^  "(" ^ String.concat ", " translated_args ^ ")", op)
      | Subscript (l, i) as op -> let index = (expr_of_string context) (_string_of_expr i ^ " - 1") in
        _string_of_expr ~parent: op l ^ "[" ^ (_string_of_expr index) ^ "]", op
      | Value v as op -> Value.to_string v, op
    in match parent with
    | Some parent when precedence (expr_to_op parent) > precedence (expr_to_op op) -> "(" ^ str ^ ")"
    | _ -> str
  in _string_of_expr expr
