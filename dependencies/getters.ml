(*#mod_use "structures.ml";;
#load "str.cma";;
#mod_use "type_operations.ml" *)

open Str
open Structures
open Errors

(* Returns the line number corresponding to the current index in the code *)
let rec get_line_no code index =
  if index < 0 then
    1
  else
    (if index < String.length code && code.[index] = '\n' then 1 else 0) + get_line_no code (index - 1)

let rec ignore_chrs code i =
  if i < (String.length code) then
    match code.[i] with
      ' ' | '\n' | '\t' | '\r' | '(' | ')' | ',' -> ignore_chrs code (i + 1)
    | _-> i
  else
    String.length code

let get_word code i =
  let len = String.length code in
  let rec _get_word i word =
    if i < len then
      match code.[i] with
      | ' ' | '\n' | '(' | ',' | ')' ->
        if word = "" then
          _get_word (i + 1) word
        else
          word, i + 1
      | x -> _get_word (i + 1) (word ^ (String.make 1 x))
    else
      word, len
  in _get_word i ""

let get_word_and_returns code i =
  let len = String.length code in
  let rec _get_word i word =
    if i < len then
      match code.[i] with
      | ' ' | '\t' when word = "" -> _get_word (i + 1) word
      | ' ' | '(' | ',' | ')' -> word, i + 1
      | x -> _get_word (i + 1) (word ^ (String.make 1 x))
    else
      word, len
  in _get_word i ""


let get_expression code index terminator =
  if string_match (regexp ("\\([^\n]*[) ]\\)\\(" ^ terminator ^ "\\)")) code index then
    matched_group 1 code, group_end 2 + 1
  else
    raise_syntax_error ~line: (get_line_no code index) ("Missing token '" ^ terminator ^ "'")

let get_line code i =
  let len = String.length code in
  let rec _get_line i line =
    if i < len then
      match code.[i] with
        '\n' -> line, i + 1
      | x -> _get_line (i + 1) (line ^ (String.make 1 x))
    else
      line, len
  in _get_line i ""

(*Check if the specified type is a valid builtin type, if that is the case, returns the right type *)
let get_type code index =
  let t, i = get_word code index in
  i, try_update_err (get_line_no code index) (fun () -> Type.of_string t)

(*Gets the parameters of the function or the procedure*)
let get_param vars code index =
  let rec _get_params ?(is_first = false) vars names index = function
    | [] -> names, index, vars
    | h :: t -> let i, type_ = get_type h 0 in
      let name, i = get_word h i in
      let sep = if is_first then "" else ", " in
      _get_params (StringMap.add (String.trim name) type_ vars) (names ^ sep ^ name) (index + i + 1) t in
  let r = regexp {|\([a-z]+ [A-Za-z_][A-Za-z0-9_]*\(, ?[a-z]+ [A-Za-z_][A-Za-z0-9_]*\)*\))|} in
  if string_match r code index then
    _get_params vars "" (index + 1) (split (regexp ",") (matched_group 1 code)) ~is_first: true
  else if code.[index] = ')' then
    "", index + 2, vars
  else
    raise_syntax_error ~line: (get_line_no code index) "Invalid function definition"

let get_var name vars =
  try StringMap.find (String.trim name) vars
  with Not_found -> raise_name_error ("Unknown variable " ^ name)
