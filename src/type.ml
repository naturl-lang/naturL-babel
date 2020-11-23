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
  | Union of t list

let rec to_string : t -> string = function
  | Int -> "entier"
  | Float -> "réel"
  | Char -> "caractère"
  | String -> "chaîne"
  | Bool -> "booléen"
  | List Any -> "liste"
  | List t ->
    let str = to_string t in
    let de = match str.[0] with
      | 'a' | 'e' | 'i' | 'o' | 'u' -> " d'"
      | _ -> " de "
    in
    let s = match t with
      | Union _ | Custom _ | List _ -> ""
      | _ -> "s"
    in
    "liste" ^ de ^ str ^ s
  | Function (params, return) -> (match return with
        None -> if params = [] then "procédure"
        else "procédure: " ^ String.concat " x " (List.map (fun t -> "(" ^ (to_string t) ^ ")" ) params)
      | _ -> "fonction: " ^ (if params = [] then "_" else String.concat " x " (List.map to_string params)) ^ " -> " ^ to_string return)
  | None -> "Ø"
  | Any -> "indéterminé"
  | Class _ -> "type"
  | Custom name -> name
  | Union types -> types |> List.map to_string |> String.concat " | "

let rec of_string str : t =
  let str = String.trim str in
  match String.split_on_char '|' str with
  | _ :: _ :: _ as list -> print_endline str; Union (list |> List.map (fun string ->
      let string = String.trim string in
      if string = "" then
        raise_syntax_error "Erreur de syntaxe dans l'union de types"
      else
        of_string string
    ))
  | _ ->
    let splitted = List.map (fun s -> String.split_on_char '_' s)
        (String.split_on_char ' ' str) in
    match List.flatten splitted with
    | "entier" :: [] -> Int
    | ("réel" | "reel") :: [] -> Float
    | ("caractere" | "caractère") :: [] -> Char
    | ("chaine" | "chaîne") :: [] -> String
    | ("booleen" | "booléen") :: [] -> Bool
    | "liste" :: [] -> List Any
    | "liste" :: "de" :: t -> List (of_string (String.concat " " t))
    | ("procédure" | "procedure") :: [] -> Function ([], None)
    | ("procédure:" | "procedure") :: tail ->
      Function (String.split_on_char 'x' (String.concat " " tail)
                |> List.map (fun s -> let s = String.trim s in
                              of_string (String.sub s 1 (String.length s - 2))),
                None)
    | "fonction:" :: tail ->
      (match Str.split (Str.regexp " -> ") (String.concat " " tail) with
       | params :: return :: [] ->
         Function ((if String.trim params = "_" then []
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
  | t, Union types | Union types, t -> List.mem t types
  | _ -> false
