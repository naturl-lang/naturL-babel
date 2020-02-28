(*#mod_use "structures.ml";; *)

open Structures ;;


(*Converts string to Type._type*)
let string_to_type type_ =
  match type_ with
    "entier" -> Type.Int
  | "reel" -> Type.Float
  | "octet" -> failwith "[Type_Error]: Type byte not implemented"
  | "booleen" -> Type.Boolean
  | "caractere" -> Type.Char
  | "chaine" -> Type.String
  | x -> failwith ("get_type: Undefined type: " ^ x) ;;

(*Check if the specified type is a valid builtin type, if that is the case, returns the right type *)
let get_type code index =
  let (t, index) = get_word code "" index in
  (* print_string t; print_string "\n"; Debug*)
  (index, string_to_type t) ;;

(*Check if the provided word is a type*)
let is_type word =
  match word with
    "entier" | "reel" | "booleen" | "caractere" | "chaine" -> true
  | _ -> false ;;
