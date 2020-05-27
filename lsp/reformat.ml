let contains s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false;;

let strip string =
  let list = Str.split (Str.regexp "[\n\r\t]+") string in
  let rec remove_trailing_spaces list = match list with
    | [] -> []
    | h :: t -> (String.trim h) :: (remove_trailing_spaces t)
  in remove_trailing_spaces list;;

let white_space_simple_separation string =
  let rec _white_space_simple_separation = function
    | [] -> ""
    | h :: [] -> h
    | h :: t -> h ^ " " ^ (_white_space_simple_separation t)
  in _white_space_simple_separation (Str.split (Str.regexp "[ ]+") string);;

let reformat_function_definition string =
  let token_list = Str.full_split (Str.regexp "[,]+") string in
  let rec _reformat_function_definition token_list = match token_list with
    | [] -> ""
    | (Str.Text a) :: t -> (white_space_simple_separation (String.trim a)) ^ (_reformat_function_definition t)
    | (Str.Delim _) :: t -> ", " ^ (_reformat_function_definition t)
  in _reformat_function_definition token_list;;

let reformat_operators string =
  let token_list = Str.full_split (Str.regexp "[<=>+*-/]+") string in
  let rec _reformat_operators token_list = match token_list with
    |[] -> ""
    |(Str.Text a) :: t -> (String.trim a) ^ (_reformat_operators t)
    |(Str.Delim a) :: t -> " " ^ a ^ " " ^ (_reformat_operators t)
  in _reformat_operators token_list;;

let get_indentation indentation_level tab_size insert_space =
  if indentation_level <= 0 then
	""
  else if insert_space then
    String.make (indentation_level*tab_size) ' '
  else
    String.make indentation_level '\t'

(*TODO Ajouter un reformat pour les arguments passés dans l'appel d'une fonction.*)

let reformat string tab_size insert_space =
  let token_list = strip string in
  let rec _reformat token_list indentation result = match token_list with
    | [] -> result
    | h :: t when contains h "sinon_si" || contains h "sinon" ->  _reformat t (indentation) (result ^ (get_indentation (indentation-1) tab_size insert_space) ^ (reformat_operators h) ^ "\n")
    | h :: t when contains h "procedure" || contains h "fonction" -> _reformat t (indentation+1) (result ^ (get_indentation indentation tab_size insert_space) ^ (reformat_function_definition h) ^ "\n")
    | h :: t when contains h "tant_que" || contains h "pour" || contains h "pour_chaque" || contains h "si" -> _reformat t (indentation+1) (result ^ (get_indentation indentation tab_size insert_space) ^ (reformat_operators h) ^ "\n")
    | h :: t when Str.string_match (Str.regexp "variables") h 0 || Str.string_match (Str.regexp "attributs") h 0 || Str.string_match (Str.regexp "methodes") h 0 -> _reformat t (indentation+1) (result ^ (get_indentation indentation tab_size insert_space) ^ h ^ "\n")
    | h :: t when Str.string_match (Str.regexp "fin") h 0 -> _reformat t (indentation-1) (result ^ (get_indentation (indentation-1) tab_size insert_space) ^ h ^ "\n")
    | h :: t when Str.string_match (Str.regexp "debut") h 0 -> _reformat t (indentation-1) (result ^ (get_indentation (indentation-2) tab_size insert_space) ^ h ^ "\n")
    | h :: t -> _reformat t indentation (result ^ (get_indentation indentation tab_size insert_space) ^ (reformat_operators h) ^ "\n")
  in _reformat token_list 0 "";;
