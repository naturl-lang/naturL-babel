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
    |[] -> ""
    |(Str.Text a) :: t -> (white_space_simple_separation (String.trim a)) ^ (_reformat_function_definition t)
    |(Str.Delim _) :: t -> ", " ^ (_reformat_function_definition t)
  in _reformat_function_definition token_list;;

let reformat_operators string =
  let token_list = Str.full_split (Str.regexp "[+*/]+") string in
  let rec _reformat_operators token_list = match token_list with
    |[] -> ""
    |(Str.Text a) :: t -> (String.trim a) ^ (_reformat_operators t)
    |(Str.Delim a) :: t -> " " ^ a ^ " " ^ (_reformat_operators t)
  in _reformat_operators token_list;;


let rec get_indentation indentation_level = match indentation_level with
  | 0 -> ""
  | x when x < 0 -> failwith "Negative indentation level"
  | _ -> "  " ^ get_indentation (indentation_level-1);;

let reformat string =
  let token_list = strip string in
  let rec _reformat token_list indentation result = match token_list with
    | [] -> result
    | h :: t when contains h "procedure" || contains h "fonction" -> _reformat t (indentation+1) (result ^ (get_indentation indentation) ^ (reformat_function_definition h) ^ "\n")
    | h :: t when Str.string_match (Str.regexp "variables") h 0 -> _reformat t (indentation+1) (result ^ (get_indentation indentation) ^ h ^ "\n")
    | h :: t when Str.string_match (Str.regexp "fin") h 0 -> _reformat t (indentation-1) (result ^ (get_indentation (indentation-1)) ^ h ^ "\n")
    | h :: t when Str.string_match (Str.regexp "debut") h 0 -> _reformat t (indentation) (result ^ (get_indentation (indentation-1)) ^ h ^ "\n")
    | h :: t -> _reformat t indentation (result ^ (get_indentation indentation) ^ (reformat_operators h) ^ "\n")
  in _reformat token_list 0 "";;
