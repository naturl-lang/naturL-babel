open Syntax
open Errors

type t =
  | Int of Big_int.big_int
  | Float of float
  | Char of char
  | String of string
  | Bool of bool
  | Variable of string
  | Instance of string * string
  | None

let to_string = function
    Int i -> Big_int.string_of_big_int i
  | Float f -> string_of_float f
  | Char c -> "'" ^ String.make 1 c ^ "'"
  | String s -> {|"|} ^ s ^ {|"|}
  | Bool b -> if b then "vrai" else "faux"
  | Variable name -> resolve_py_conficts name
  | Instance (type_def, name) -> resolve_py_conficts type_def ^ "." ^ (resolve_py_conficts name)
  | None -> "rien"

let of_string = function str ->
  let str = String.trim str
  and name_re = "[_a-zA-Z\\.][_a-zA-Z0-9\\.]*" in
  if Str.string_match (Str.regexp "^-?[0-9]+$") str 0 then
    Int (Big_int.big_int_of_string str)
  else if Str.string_match (Str.regexp {|^[0-9]+.[0-9]*$|}) str 0 then
    Float (float_of_string str)
  else if Str.string_match (Str.regexp {|^'\(.\)'$|}) str 0 then
    Char (Str.matched_group 1 str).[0]
  else if Str.string_match (Str.regexp {|^"\(.*\)"$|}) str 0 then
    String (Str.matched_group 1 str)
  else if str = "vrai" || str = "faux" then
    Bool (str = "vrai")
  else if str = "nul" then
    None
  else if Str.string_match (Str.regexp ("^" ^ name_re ^ "$")) str 0 then
    Variable str
  else if Str.string_match (Str.regexp ("^instance " ^ name_re ^ "$")) str 0 then
    Instance ("self", String.sub str 9 (String.length str - 9))
  else if str = "" then
    raise_syntax_error "Une opérande est attendue"
  else
    raise_syntax_error "Expression invalide"

let get_type = function
    Int _ -> Type.Int
  | Float _ -> Type.Float
  | Char _ -> Type.Char
  | String _ -> Type.String
  | Bool _ -> Type.Bool
  | Variable _ -> assert false
  | Instance _ -> raise_type_error "Not supported yet"
  | None -> Type.None
