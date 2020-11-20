open Utils
open Errors

type t =
  | Int
  | Float
  | Char
  | String
  | Bool
  | List of t
  | Function of t list * t
  | None
  | Any
  | Class of t StringMap.t * bool StringMap.t  (* attributes and methodes *)
  | Custom of string (* name of the class *)

let rec to_string : t -> string = function
  | Int -> "entier"
  | Float -> "reel"
  | Char -> "caractere"
  | String -> "chaine"
  | Bool -> "booleen"
  | List Any -> "liste"
  | List t -> "liste de " ^ to_string t
  | Function (params, return) -> (match return with
        None -> if params = [] then "procedure"
        else "procedure: " ^ String.concat " x " (List.map (fun t -> "(" ^ (to_string t) ^ ")" ) params)
      | _ -> "fonction: " ^ (if params = [] then "_" else String.concat " x " (List.map to_string params)) ^ " -> " ^ to_string return)
  | None -> "Ø"
  | Any -> "?"
  | Class _ -> "type"
  | Custom name -> name

let rec of_string str : t =
  let splitted = List.map (fun s -> String.split_on_char '_' s)
      (String.split_on_char ' ' (String.trim str)) in
  match List.flatten splitted with
  | "entier" :: [] -> Int
  | "reel" :: [] -> Float
  | "caractere" :: [] -> Char
  | "chaine" :: [] -> String
  | "booleen" :: [] -> Bool
  | "liste" :: [] -> List Any
  | "liste" :: "de" :: t -> List (of_string (String.concat " " t))
  | ("procedure:" | "procedure") :: [] -> Function ([], None)
  | "procedure:" :: tail -> Function (String.split_on_char 'x' (String.concat " " tail)
                                       |> List.map (fun s -> let s = String.trim s in
                                                     of_string (String.sub s 1 (String.length s - 2))),
                                       None)
  | "fonction:" :: tail -> (match Str.split (Str.regexp " -> ") (String.concat " " tail) with
        params :: return :: [] -> Function ((if String.trim params = "_" then []
                                              else
                                                (String.split_on_char 'x' params)
                                                |> List.map (fun s -> let s = String.trim s in
                                                              of_string (String.sub s 1 (String.length s - 2)))),
                                             of_string return)
      | _ -> raise_name_error ("Le type '" ^ str ^ "' est inconnu"))
  | ("Ø" | "rien") :: [] -> None
  | "?" :: [] -> Any
  | t :: [] -> raise_name_error ("Le type '" ^ t ^ "n'est pas défini")
  | _ -> raise_name_error ("Le type '" ^ str ^ "' n'est pas défini")

let get_iterable_type : t -> t option = function
    String -> Some Char
  | List t -> Some t
  | _ -> None

let print_vars = StringMap.iter (function name -> function t ->
    print_endline ("var " ^ name ^ " : " ^ to_string t))

let rec equal type1 type2 =
  match type1, type2 with
  | t1, t2 when t1 = t2 -> true
  | Any, _ | _, Any -> true
  | List t1, List t2 -> equal t1 t2
  | Function (list1, ret1), Function (list2, ret2) ->
    ret1 = ret2 && List.for_all2 equal list1 list2
  | Custom s1, Custom s2 -> s1 = s2
  | _ -> false
