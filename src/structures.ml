open Utils
open Errors

module Type = struct

  type t = [
    | `Int
    | `Float
    | `Char
    | `String
    | `Bool
    | `List of t
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
    | `List `Any -> "liste"
    | `List t -> "liste de " ^ to_string t
    | `Function (params, return) -> (match return with
          `None -> "procedure: " ^ String.concat " x " (List.map to_string params)
        | _ -> "fonction: " ^ String.concat " x " (List.map to_string params) ^ " -> " ^ to_string return)
    | `None -> "Ø"
    | `Any -> "?"

  let rec of_string str : t =
    let splitted = List.map (fun s -> String.split_on_char '_' s)
        (String.split_on_char ' ' (String.trim str)) in
    match List.flatten splitted with
    | "entier" :: [] -> `Int
    | "reel" :: [] -> `Float
    | "caractere" :: [] -> `Char
    | "chaine" :: [] -> `String
    | "boolean" :: [] -> `Bool
    | "liste" :: [] -> `List `Any
    | "liste" :: "de" :: t -> `List (of_string (String.concat " " t))
    | "procedure" :: params  :: [] -> `Function (List.map of_string (String.split_on_char 'x' params), `None)
    | "fonction:" :: tail -> (match Str.split (Str.regexp " -> ") (String.concat " " tail) with
          params :: return :: [] -> `Function (List.map of_string (String.split_on_char 'x' params), of_string return)
        | _ -> raise_name_error ("Unknown type '" ^ str ^ "'"))
    | ("Ø" | "rien") :: [] -> `None
    | "?" :: [] -> `Any
    | _ -> raise_name_error ("Unknown type '" ^ str ^ "'")

  let get_iterable_type : t -> t option = function
      `String -> Some `Char
    | `List t -> Some t
    | _ -> None

  let rec is_compatible (type1: t) (type2: t) =
    type2 = `Any || match type1 with
    | `Any -> true
    | `Int | `Float | `Char | `String | `Bool | `None -> type1 = type2
    | `List t1 -> (match type2 with
        | `List t2 -> is_compatible t1 t2
        | _ -> false)
    | `Function (params1, return1) -> (match type2 with
        | `Function (params2, return2) -> List.for_all2 is_compatible params1 params2 && is_compatible return1 return2
        | _ -> false)

end

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
    else if str = "vrai" || str = "faux" then
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
    | Subscript of t * t      (* list[x] *)
    | Value of Value.t        (* x *)
end


type scope =
  | If
  | Else
  | While
  | For
  | Function
  | Function_definition

(* The current context of the code *)
type context = {
  code: string;                    (* The whole code *)
  index: int;                      (* The index *)
  vars: Type.t StringMap.t;        (* The set of known variables *)
  scopes: scope list;              (* The stack of scopes *)
}