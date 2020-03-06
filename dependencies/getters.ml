(*#mod_use "structures.ml";;
  #load "str.cma";; (*UNCOMMENT FOR TOPLEVEL*)
  #mod_use "type_operations.ml" *)

open Structures;;


let rec ignore_chrs code i =
  if i < (Buffer.length code) then
    match Buffer.nth code i with
      ' ' | '\n' | '\t' | '\r' | '(' | ')' | ',' -> ignore_chrs code (i + 1)
    | _-> i
  else
    Buffer.length code ;;

let get_word code i =
  let len = Buffer.length code in
  let rec _get_word i word =
    if i < len then
      match Buffer.nth code i with
      | ' ' | '\t' when word = "" -> _get_word (i + 1) word
      | ' ' | '\n' | '(' | ',' | ')' -> word, i + 1
      | x -> _get_word (i + 1) (word ^ (String.make 1 x))
    else
      word, len
  in _get_word i "" ;;

let get_word_and_returns code i =
  let len = Buffer.length code in
  let rec _get_word i word =
    if i < len then
      match Buffer.nth code i with
      | ' ' | '\t' when word = "" -> _get_word (i + 1) word
      | ' ' | '(' | ',' | ')' -> word, i + 1
      | x -> _get_word (i + 1) (word ^ (String.make 1 x))
    else
      word, len
  in _get_word i "" ;;


let get_expression code index terminator =
  let i = index in
  let rec _get_expression code index terminator =
    match get_word code index with
    | t, k when t = terminator -> (Buffer.sub code i (k - String.length terminator), k+1)
    | ("\n", _) |("\r", _) -> failwith("Syntax Error : no " ^ terminator ^ " was found on the line")
    | (_, i) when i >= Buffer.length code -> failwith("Syntax Error : code was not supposed to end here")
    | _ -> _get_expression code (index + 1) terminator
  in _get_expression code index terminator


let get_line code i =
  let len = Buffer.length code in
  let rec _get_line i line =
    if i < len then
      match Buffer.nth code i with
        '\n' -> line, i + 1
      | x -> _get_line (i + 1) (line ^ (String.make 1 x))
    else
      line, len
  in _get_line i "" ;;

(*Check if the specified type is a valid builtin type, if that is the case, returns the right type *)
let get_type code index =
  let (t, index) = get_word code index in
  (* print_string t; print_string "\n"; Debug*)
  (index, Type.type_of_string t) ;;

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
    let i, type_struct = get_type code (ignore_chrs code i) in
    let name, i = get_word code (ignore_chrs code i) in
    get_param (VarSet.add ({name; type_struct}) vars) (variables ^ name ^ ", ")
      code (ignore_chrs code i) ;;


let rec get_var_by_name var_name var_list =
  match var_list with
    [] -> failwith ("get_var_by_name: var: "^var_name^" not found in the current local variables")
  | var :: _ when var.name = var_name -> var
  | _ :: r -> get_var_by_name var_name r ;;
