open Str
open Utils
open Errors
open Structures

(* Returns the line number corresponding to the current index in the code *)
let rec get_line_no code index =
  if index < 0 then
    0
  else
    (if index < String.length code && code.[index] = '\n' then 1 else 0) + get_line_no code (index - 1)

let rec get_col_no code index =
  if index <= 0 then
    1
  else if index >= String.length code then
    get_col_no code (index - 1)
  else if code.[index] = '\n' then
    0
  else
    1 + get_col_no code (index - 1)

let get_last_line code =
  get_line_no code (String.length code - 1)


(* This function separates the code source in a list of lines and acceses the line_number'nth list
   to get in a form of a tuple the starting column and the ending column*)
let get_line_column_data line_number line_list =
  let line_string = List.nth line_list line_number  in
  let size = String.length line_string in
  if line_string = "" then
    0, 0
  else
    let rec _get_line_first_char_data index =
      if index = size || line_string.[index] != ' ' then
        index
      else
        _get_line_first_char_data (index+1)
    in
    let rec _get_line_last_char_data index =
      if index = 0 || line_string.[index] != ' ' then
        index
      else
        _get_line_last_char_data (index-1)
    in
    _get_line_first_char_data 0, _get_line_last_char_data (size-1)



let get_current_line_location ?line_list code index : Location.t =
  let line_no = get_line_no code index in
  let line_list = match line_list with
    | Some list -> list
    | None -> String.split_on_char '\n' code in
  let column_start_index, column_end_index = get_line_column_data line_no line_list
  in
  Location.{
    line = line_no+1;
    range = {
      start = column_start_index + (if column_start_index + column_end_index = 0 then 0 else 1);
      end_ = column_end_index + (if column_start_index + column_end_index = 0 then 0 else 1);
    }
  }

let get_last_line_location code =
  get_current_line_location code (String.length code - 1)

let get_location code start_index end_index : Location.t =
  {
    line = get_line_no code start_index;
    range = {
      start = get_col_no code start_index;
      end_ = get_col_no code end_index
    };
  }


let rec ignore_chrs code i =
  if i < (String.length code) then
    match code.[i] with
      ' ' | '\n' | '\t' | '\r' | '(' | ')' | ',' -> ignore_chrs code (i + 1)
    | _-> i
  else
    raise_syntax_error "La lecture s'est arrêtée puisque la fin du fichier a été atteinte"
      ~location:(get_last_line_location code)

let rec ignore_spaces code i =
  if i < (String.length code) then
    if code.[i] = ' ' then
      ignore_spaces code (i + 1)
    else
      i
  else
    raise_syntax_error "La lecture s'est arrêtée puisque la fin du fichier a été atteinte"
      ~location:(get_last_line_location code)

let get_word code i =
  let len = String.length code in
  let rec _get_word i word =
    if i < len then
      match code.[i] with
        ' ' | '\n' | '\t' | '\r' | '(' | ',' | ')' ->
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
  if string_match (regexp ("\\(\\(.+ *| *\n\\)*[^\n]+\\)\\(" ^ terminator ^ "\\)")) code index then
    matched_group 1 code |> replace_string "|" "" |> replace_string "\n" "", group_end 3 + 1
  else
    raise_syntax_error ("Il manque le mot-clé '" ^ terminator ^ "'")
      ~location:(get_last_line_location code)

let get_line code i =
  let len = String.length code in
  let rec _continue_to_next_line index =
    if index + 1 >= len || code.[index + 1] <> '\n' then
      index
    else
      _continue_to_next_line (index + 1)
  in
  let rec _get_line escaped i line =
    if i < len then
      match code.[i] with
        '\n' -> if escaped || String.trim line = "" then
          _get_line false (i + 1) line
        else line, _continue_to_next_line (i + 1)
      | '|' -> _get_line true (i + 1) line
      | x -> if escaped && not (List.mem x [' '; '\t']) then
          raise_syntax_error ("Caractère '" ^ (String.make 1 x) ^ "' inattendu")
        else
          _get_line escaped (i + 1) (line ^ (String.make 1 x))
    else
      line, len
  in _get_line false i ""

(*Check if the specified type is a valid builtin or user-defined type, if that is the case, returns the right type *)
let get_type code index =
  let t, i = get_word code index in
  i, try_update_err (get_location code index i) (fun () -> Type.of_string t)
