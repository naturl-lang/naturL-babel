(*#mod_use "structures.ml";;
  #load "str.cma";; (*UNCOMMENT FOR TOPLEVEL*)
  #mod_use "type_operations.ml" *)
open Type_operations;;
open Structures;;
open Str;;


let rec ignore_chrs code i =
  if i < (Buffer.length code) then
    match Buffer.nth code i with
      ' ' | '\n' | '\t' | '\r' | '(' | ')' | ',' -> ignore_chrs code (i + 1)
    | _-> i
  else
    Buffer.length code ;;

let rec get_word code word i  =
  let l = Buffer.length code in
  if i < l then
    match Buffer.nth code i with
      ' ' | '\n' | '(' | ',' | ')' -> (word, i)
    | _ when i = Buffer.length code -> (word, i)
    | x -> get_word code (word ^ (Char.escaped x)) (i + 1)
  else
    (word, l + 1) ;;

(*Gets the following tokens as part of a type char*)
let get_char code i =
  (*Buffer nth error if the char gives an EOF error*)
  if Buffer.nth code i <> (Char.chr 39) || Buffer.nth code (i + 2) <> (Char.chr 39) then
    failwith "get_char: the given value is not a char"
  else
    (Char.escaped (Buffer.nth code (i + 1)), i + 3)
;;

(*Gets the following tokens as part of a type string*)
let get_str code i =
  (*Buffer nth error if the given str gives an EOF error*)
  if Buffer.nth code i <> '"' then
    failwith "get_str: the given value is not a string"
  else
    let rec _get_str i result =
      let c = Buffer.nth code i in
      if c = '"' then
        (result, i + 1)
      else
        _get_str (i + 1) (result ^ (Char.escaped c))
    in
    _get_str (i + 1) "" ;;

(*Gets the parameters of the function or the procedure*)
let rec get_param vars variables code i =
  if i = Buffer.length code then
    failwith "get_vars: Unclosed ("
  else if Buffer.nth code i = ':' then
    (vars, String.sub variables 0 (String.length variables - 2), i)
  else
    let i, type_ = get_type code (ignore_chrs code i) in
    let name, i = get_word code "" (ignore_chrs code i) in
    get_param ({name = name; _type = type_; value = "None"; is_parametre = true} :: vars) (variables ^ name ^ ", ")
      code (ignore_chrs code i) ;;


let rec get_var_by_name var_name var_list =
  match var_list with
    [] -> failwith ("get_var_by_name: var: "^var_name^" not found in the current local variables")
  | var :: _ when var.name = var_name -> var
  | _ :: r -> get_var_by_name var_name r ;;
