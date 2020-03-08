(*#mod_use "structures.ml";;
  #load "str.cma";; (*UNCOMMENT FOR TOPLEVEL*)
  #mod_use "type_operations.ml" *)

open Str
open Structures

let rec ignore_chrs code i =
  if i < (Buffer.length code) then
    match Buffer.nth code i with
      ' ' | '\n' | '\t' | '\r' | '(' | ')' | ',' -> ignore_chrs code (i + 1)
    | _-> i
  else
    Buffer.length code

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
  in _get_word i ""

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
  in _get_word i ""


let get_expression code index terminator =
  let str = Buffer.contents code in
  if string_match (regexp ({|\([^\n]*[) ]\)\(|} ^ terminator ^ "\\)")) str index then
    matched_group 1 str, group_end 2 + 1
  else
    failwith ("Syntax error: Missing token '" ^ terminator ^ "'")

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
let get_param vars code index =
  let rec _get_params ?(is_first = false) vars names index = function
    | [] -> names, index, vars
    | h :: t -> let buf = Buffer.create (String.length h) in
      Buffer.add_string buf h;
      let i, type_struct = get_type buf 0 in
      let name, i = get_word buf i in
      let sep = if is_first then "" else ", " in
      _get_params (VarSet.add {name; type_struct} vars) (names ^ sep ^ name) (index + i + 1) t in
  let content = Buffer.contents code
  and r = regexp {|\([a-z]+ [A-Za-z_][A-Za-z0-9_]*\(, ?[a-z]+ [A-Za-z_][A-Za-z0-9_]*\)*\))|} in
  if string_match r content index then
    _get_params vars "" (index + 1) (split (regexp ",") (matched_group 1 content)) ~is_first: true
  else
    failwith "Syntax error on function definition"

let rec get_var_by_name var_name var_list =
  match var_list with
    [] -> failwith ("get_var_by_name: var: " ^ var_name ^ " not found in the current local variables")
  | var :: _ when var.name = var_name -> var
  | _ :: r -> get_var_by_name var_name r ;;
