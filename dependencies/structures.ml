open Errors

module Type = struct

  type t = [
    | `Int
    | `Float
    | `Char
    | `String
    | `Bool
    | `List of t
    | `Variable of t
    | `Function of t list * t
    | `None
    | `Any
  ]

  let rec to_string : t -> string = function
    | `Int -> "entier"
    | `Float -> "reel"
    | `Char -> "caractere"
    | `String -> "chaine"
    | `Bool -> "boolean"
    | `List t -> "liste de " ^ to_string t
    | `Variable t -> "variable : " ^ (to_string t)
    | `Function (params, return) ->
      "fonction : " ^ String.concat " x " (List.map to_string params) ^ " -> " ^ to_string return
    | `None -> "rien"
    | `Any -> "-"

  let rec of_string str =
    match String.split_on_char ' ' (String.trim str) with
    | "entier" :: [] -> `Int
    | "reel" :: [] -> `Float
    | "caractere" :: [] -> `Char
    | "chaine" :: [] -> `String
    | "boolean" :: [] -> `Bool
    | "liste" :: "de" :: t -> `List (of_string (String.concat " " t))
    | "variable" :: ":" :: t :: [] -> `Variable (of_string t)
    | "fonction" :: ":" :: tail -> (match Str.split (Str.regexp " -> ") (String.concat " " tail) with
        | params :: return :: [] -> `Function (List.map of_string (String.split_on_char 'x' params), of_string return)
        | _ -> raise_name_error ("Unknown type '" ^ str ^ "'"))
    | "rien" :: [] -> `None
    | _ -> raise_name_error ("Unknown type '" ^ str ^ "'")

  let get_iterable_type = function
      `String -> Some `Char
    | `List t -> Some t
    | _ -> None
end


module StringMap = Map.Make(String)

let print_vars = StringMap.iter (function name -> function t ->
    print_endline ("var " ^ name ^ " : " ^ Type.to_string t))

module Value = struct

  type t =
    | Int of int
    | Float of float
    | Char of char
    | String of string
    | Bool of bool
    | Variable of string
    | None

  let to_string = function
      Int i -> string_of_int i
    | Float f -> string_of_float f
    | Char c -> "'" ^ String.make 1 c ^ "'"
    | String s -> {|"|} ^ s ^ {|"|}
    | Bool b -> if b then "True" else "False"
    | Variable name -> name
    | None -> "None"

  let of_string = function str ->
    let str = String.trim str
    and name_re = "[_a-zA-Z][_a-zA-Z0-9]*" in
    if Str.string_match (Str.regexp "^-?[0-9]+$") str 0 then
      Int (int_of_string str)
    else if Str.string_match (Str.regexp {|^[0-9]+.[0-9]*$|}) str 0 then
      Float (float_of_string str)
    else if Str.string_match (Str.regexp {|^'\(.)\)'$|}) str 0 then
      Char (Str.matched_group 1 str).[0]
    else if Str.string_match (Str.regexp {|^"\(.*\)"$|}) str 0 then
      String (Str.matched_group 1 str)
    else if str = "vrai" || str = "false" then
      Bool (str = "vrai")
    else if Str.string_match (Str.regexp ("^" ^ name_re ^ "$")) str 0 then
      Variable str
    else
      raise_name_error ("Can not resolve operand '" ^ str ^ "'")

  let get_type vars = function
    | Int _ -> `Int
    | Float _ -> `Float
    | Char _ -> `Char
    | String _ -> `String
    | Variable name -> (try StringMap.find name vars
                        with Not_found -> raise_name_error ("Unknown variable: '" ^ name ^ "'"))
    | Bool _ -> `Bool
    | None -> `None
end

module Expr = struct

  type t =
    | Plus of t * t           (* x + y *)
    | Minus of t * t          (* x * y *)
    | Neg of t                (* -x *)
    | Times of t * t          (* x * y *)
    | Div of t * t            (* x / y *)
    | Div_int of t * t        (* x div y *)
    | Modulus of t * t        (* x mod y *)
    | Eq of t * t             (* x = y *)
    | Gt of t * t             (* x > y *)
    | Gt_eq of t * t          (* x >= y *)
    | Lt of t * t             (* x < y *)
    | Lt_eq of t * t          (* x <= y *)
    | And of t * t            (* x et y *)
    | Or of t * t             (* x ou y *)
    | Not of t                (* non x *)
    | List of t list          (* [x, y, ...] *)
    | Call of string * t list (* <function-name>(x, y, ...) *)
    | Value of Value.t        (* x *)

  open (struct
    let is_type_accepted t = function
        Plus _ -> List.mem t [`Int; `Float; `String]
      | Minus _ | Times _ | Neg _ -> List.mem t [`Int; `String]
      | Div_int _ | Modulus _ -> t = `Int
      | Div _ -> t = `Float
      | Eq _ | Gt _ | Gt_eq _ | Lt _ | Lt_eq _ -> true
      | And _ | Or _ | Not _ -> t = `Bool
      | _ -> assert false

    let rec is_list_uniform vars = function
      | h1 :: h2 :: t -> h1 = h2 && is_list_uniform vars (h2 :: t)
      | _ -> true
  end)

  let rec get_type vars = function
    | Plus (l, r) | Minus (l, r) | Times (l, r) | Div (l, r) | Div_int (l, r) | Modulus (l, r) | And (l, r) | Or (l, r) as e ->
      let l_type = get_type vars l and r_type = get_type vars r in
      if l_type = r_type && is_type_accepted l_type e then l_type else
        raise_type_error ("Invalid operation for expressions of type " ^ (Type.to_string l_type) ^ " and type " ^ (Type.to_string r_type))
    | Not arg | Neg arg as e -> let arg_type = get_type vars arg in if is_type_accepted arg_type e then arg_type else
        raise_type_error ("Invalid operation for expression of type " ^ (Type.to_string arg_type))
    | Eq (l, r) | Gt (l, r) | Gt_eq (l, r) | Lt (l, r) | Lt_eq (l, r) -> let l_type = get_type vars l and r_type = get_type vars r in
      if l_type = r_type then `Bool else
        raise_type_error ("Can't compare expressions of type " ^ (Type.to_string l_type) ^ " and type " ^ (Type.to_string r_type))
    | List [] -> `List `Any
    | List (h :: t) -> if is_list_uniform vars (List.map (get_type vars) (h :: t)) then `List (get_type vars h)
      else raise_name_error ("All elements of a list must have the same type")
    | Call (name, params) ->
      (try (match StringMap.find name vars with
           | `Function (p, return) as f -> let params_type = List.map (get_type vars) params in
             if params_type = p then return
             else raise_unexpected_type_error (Type.to_string f) (Type.to_string (`Function (params_type, `Any)))
           | _ as t -> raise_type_error ("Variables of type " ^ (Type.to_string t) ^ " are not callable"))
       with Not_found -> raise_name_error ("Unknown function: '" ^ name ^ "'"))
    | Value v -> Value.get_type vars v
end

type scope =
  | If
  | Else
  | While
  | For
  | Function

(* The current context of the code *)
type context = {
  code: string;                    (* The whole code *)
  index: int;                      (* The index *)
  vars: Type.t StringMap.t;        (* The set of known variables *)
  scopes: scope list;              (* The stack of scopes *)
  imports: string list;            (* The list of imports that need to be added at the beginning of the code *)
}
