open Utils
open Errors

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
  | `Class of t StringMap.t * bool StringMap.t  (* attributes and methodes *)
  | `Custom of string (* name of the class *)
]

let rec to_string : t -> string = function
  | `Int -> "entier"
  | `Float -> "reel"
  | `Char -> "caractere"
  | `String -> "chaine"
  | `Bool -> "booleen"
  | `List `Any -> "liste"
  | `List t -> "liste de " ^ to_string t
  | `Function (params, return) -> (match return with
        `None -> if params = [] then "procedure"
        else"procedure: " ^ String.concat " x " (List.map (fun t -> "(" ^ (to_string t) ^ ")" ) params)
      | _ -> "fonction: " ^ (if params = [] then "_" else String.concat " x " (List.map to_string params)) ^ " -> " ^ to_string return)
  | `None -> "Ø"
  | `Any -> "?"
  | `Class _ -> "type"
  | `Custom name -> name

let rec of_string (vars: t StringMap.t) str : t =
  let splitted = List.map (fun s -> String.split_on_char '_' s)
      (String.split_on_char ' ' (String.trim str)) in
  match List.flatten splitted with
  | "entier" :: [] -> `Int
  | "reel" :: [] -> `Float
  | "caractere" :: [] -> `Char
  | "chaine" :: [] -> `String
  | "booleen" :: [] -> `Bool
  | "liste" :: [] -> `List `Any
  | "liste" :: "de" :: t -> `List ((of_string vars) (String.concat " " t))
  | ("procedure:" | "procedure") :: [] -> `Function ([], `None)
  | "procedure:" :: tail -> `Function (String.split_on_char 'x' (String.concat " " tail)
                                       |> List.map (fun s -> let s = String.trim s in
                                                     (of_string vars) (String.sub s 1 (String.length s - 2))),
                                       `None)
  | "fonction:" :: tail -> (match Str.split (Str.regexp " -> ") (String.concat " " tail) with
        params :: return :: [] -> `Function ((if String.trim params = "_" then []
                                              else
                                                (String.split_on_char 'x' params)
                                                |> List.map (fun s -> let s = String.trim s in
                                                              (of_string vars) (String.sub s 1 (String.length s - 2)))),
                                             of_string vars return)
      | _ -> raise_name_error ("Le type '" ^ str ^ "' est inconnu"))
  | ("Ø" | "rien") :: [] -> `None
  | "?" :: [] -> `Any
  | t :: [] -> (match StringMap.find_opt t vars with
      | Some (`Class _) -> `Custom t
      | _ -> raise_name_error ("Le type '" ^ str ^ "' est inconnu"))
  | _ -> raise_name_error ("Le type '" ^ str ^ "' est inconnu")

let get_iterable_type : t -> t option = function
    `String -> Some `Char
  | `List t -> Some t
  | _ -> None

let rec is_compatible (expected: t) (actual: t) =
  expected = `Any || match actual with
  | `Any -> false
  | `None -> true
  | `Int -> expected = `Int || expected = `Float
  | `Char -> expected = `Char || expected = `String
  | `Float | `String | `Bool | `Class _ -> expected = actual
  | `List t1 -> (match expected with  (* A list is covariant *)
      | `List t2 -> is_compatible t2 t1
      | _ -> false)
  | `Function (params1, return1) -> (match expected with  (* A function is covariant in its return type contravariant in its argument type *)
      | `Function (params2, return2) -> List.for_all2 is_compatible params1 params2 && is_compatible return2 return1
      | _ -> false)
  | `Custom _ -> to_string expected = to_string actual

let get_attr_meths name (vars:t StringMap.t) =
  match (StringMap.find name vars) with
  |`Class (attr_meths, are_set) -> attr_meths, are_set
  | _ -> failwith "Internal missuse of get_attr_meth, No class in context.vars"

let print_vars = StringMap.iter (function name -> function t ->
    print_endline ("var " ^ name ^ " : " ^ to_string t))
