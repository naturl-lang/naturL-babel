(*#load "str.cma";;*)

type token =
  |Litteral of string
  |Identifier of string
  |Operator of string
  |OpenP
  |CloseP
  |Coma
  |OpenHook
  |CloseHook;;

let print_token token =
  let token_to_string = function
    |Litteral a -> "Litteral " ^ a
    |Identifier a -> "Identifier " ^ a
    |Operator a -> "Operator " ^ a
    |OpenP -> "OpenP"
    |CloseP -> "CloseP"
    |Coma -> "Coma"
    |OpenHook -> "OpenHook"
    |CloseHook -> "CloseHook"
  in print_endline (token_to_string token);;

let print_tokens tokens = List.iter print_token tokens;;

let tokenize input =
  let reg_identifier = Str.regexp "[a-zA-Z]+[0-9]*" and
  reg_boolean = Str.regexp "vrai\\|faux" and
  reg_number = Str.regexp "[0-9]+\\.?[0-9]*" and
  reg_operator = Str.regexp "ou\\|et\\|=\\|<=\\|>=\\|<\\|>\\|*\\|+\\|-" and
  reg_string = Str.regexp  {|"\([^"]\|\"\)*"|}  and
  reg_char = Str.regexp "'[a-Za-z0-9]*'" and
  reg_openp = Str.regexp "(" and
  reg_closep = Str.regexp ")" and
  reg_coma = Str.regexp "," and
  reg_openhook = Str.regexp "\\[" and
  reg_closehook = Str.regexp "\\]" and length = String.length input  in
  let rec _tokenize input index =
  if index = length then
    []
  else
  if input.[index] = ' ' then
    _tokenize input (index+1)
  else
  if Str.string_match reg_operator input index then
    let token = Str.matched_string input in
    (Operator token) :: _tokenize input (index + (String.length token))
  else if Str.string_match reg_number input index || Str.string_match reg_string input index
          || Str.string_match reg_boolean input index || Str.string_match reg_char input index then
    let token = Str.matched_string input in
    (Litteral token) :: _tokenize input (index + (String.length token))
  else if Str.string_match reg_identifier input index then
    let token = Str.matched_string input in
    (Identifier token) :: _tokenize input (index + (String.length token))
  else if Str.string_match reg_openp input index then
    OpenP :: _tokenize input (index+1)
  else if Str.string_match reg_closep input index then
    CloseP :: _tokenize input (index+1)
  else if Str.string_match reg_coma input index then
    Coma :: _tokenize input (index+1)
  else if Str.string_match reg_openhook input index then
    OpenHook :: _tokenize input (index+1)
  else if Str.string_match reg_closehook input index then
    CloseHook :: _tokenize input (index+1)
  else
    failwith ("Could not capture the unknown token")
  in _tokenize input 0;;
