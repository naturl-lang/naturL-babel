module StringMap = Map.Make(String)
module StringSet = Set.Make(String)


let read_file name =
  let chan = open_in name in
  let content = ref "" in
  (try
     while true do
       content := !content ^ "\n" ^ input_line chan;
     done
   with End_of_file -> close_in chan);
  !content

let read_lines name =
  let chan = open_in name in
  let lines = ref [] in
  (try
     while true do
       lines := input_line chan :: !lines
     done
   with End_of_file -> close_in chan);
  !lines

let write_file name content =
  let chan = open_out name in
  Printf.fprintf chan "%s\n" content;
  close_out chan

let write_lines name lines =
  let chan = open_out name in
  let rec __write_lines = function
    | [] -> ()
    | line :: t -> Printf.fprintf chan "%s\n" line; __write_lines t
  in __write_lines lines; close_out chan

let replace_string original replacement string =
  let rec __replace_string = function
      [] -> ""
    | h :: t when h = original -> replacement ^ __replace_string t
    | h :: t -> h ^ __replace_string t
  in __replace_string (List.init (String.length string) (String.get string) |> List.map (String.make 1))


let get_indentation depth =
  let rec _get_indentation depth =
    if depth = 0 then
      ""
    else
      "    " ^ _get_indentation (depth - 1)
  in if depth < 0 then
    failwith "Internal error: get_indentation: depth can't be negative"
  else
    _get_indentation depth

let rec find_assoc key = function
    | (k, value) :: t -> if key = k then Some value else find_assoc key t
    | [] -> None

let rec find_bad_elt default expected = function
  | [] -> default
  | h :: _ when h <> expected -> h
  | _ :: t -> find_bad_elt default expected t

let rec is_prefix word pref =
  let w_len = String.length word
  and p_len = String.length pref in
  p_len = 0 || w_len <> 0 && word.[0] = pref.[0]
               && is_prefix (String.sub word 1 (w_len - 1)) (String.sub pref 1 (p_len - 1))

(* Returns true if one word in the list starts with prefix *)
let rec is_list_prefix list pref = match list with
  | w :: t -> is_prefix w pref || is_list_prefix t pref
  | _ -> false ;;

let rec append_rev l1 l2 =
  match l2 with
  | [] -> l1
  | h :: t -> append_rev (h :: l1) t

let split_list ?(from = 0) ?(to_ = max_int) sep list =
  let rec _split_list ?(current = []) ?(i = 0) list =
    match list with
    | [] -> if current = [] then [] else [current]
    | _ when i > to_ -> if current = [] then [] else [current]
    | _ :: t when i < from -> _split_list t ~i: (i + 1)
    | h :: t when h = sep -> current :: _split_list t ~i: (i + 1)
    | h :: t -> _split_list t ~current: (current @ [h]) ~i: (i + 1)
  in _split_list list

let list_of_queue queue = List.init (Queue.length queue) (fun _ -> Queue.take queue)

(* Create a map from a list of tuples *)
let rec string_map_of_list = function
  | [] -> StringMap.empty
  | (key, value) :: t -> StringMap.add key value (string_map_of_list t)

let rec string_set_of_list = function
  | [] -> StringSet.empty
  | key :: t -> string_set_of_list t |> StringSet.add key


let fstring format =
  let f = format_of_string format in
  Printf.sprintf f

let fprint ?(oc=stdout) format =
  let f = format_of_string format in
  Printf.fprintf oc f

(* Return if a single-char string is valid inside an identifier *)
let is_id_char s =
  Str.string_match (Str.regexp "[0-9A-Za-z_]") s 0

let get_word_at_index index text =
  let n = String.length text in
  let rec _get_word_at is_in_word current_word current_index =
    if current_index >= n then
      current_word
    else
      let s = String.make 1 text.[current_index] in
      if is_id_char s then
        let word = if is_in_word then current_word ^ s else s in
        _get_word_at true word (current_index + 1)
      else if current_index < index then
        _get_word_at false current_word (current_index + 1)
      else
        current_word
  in _get_word_at false "" 0

let get_word_at_position line character text =
  let n = String.length text in
  let rec _get_word_at is_in_word current_word index current_line current_char =
    if index >= n then
      current_word
    else
      let s = String.make 1 text.[index] in
      if not (is_id_char s) && current_line >= line && current_char >= character then
        current_word
      else if s = "\n" then
        _get_word_at false current_word (index + 1) (current_line + 1) current_char
      else if current_line < line then
        _get_word_at false current_word (index + 1) current_line current_char
      else if is_id_char s then
        let word = if is_in_word then current_word ^ s else s in
        _get_word_at true word (index + 1) current_line (current_char + 1)
      else
        _get_word_at false current_word (index + 1) current_line (current_char + 1)
  in _get_word_at false "" 0 0 0

let get_index_at line character text =
  let n = String.length text in
  let rec _get_line_at line character index =
    if index >= n || line < 0 || character < 0 then raise Not_found
    else if line = 0 && character = 0 then
      index
    else if text.[index] = '\n' then
      _get_line_at (line - 1) character (index + 1)
    else if line = 0 then
      _get_line_at line (character - 1) (index + 1)
    else
      _get_line_at line character (index + 1)
  in _get_line_at line character 0

let get_line_length line text =
  let n = String.length text in
  let rec __get_line_length character length =
    if character >= n || text.[character] = '\n' then
      length
    else
      __get_line_length (character + 1) (length + 1)
  in
  let rec _get_line_length line index =
    if index > n then
      raise Not_found
    else if line = 0 then
      __get_line_length index 0
    else if text.[index] = '\n' then
      _get_line_length (line - 1) (index + 1)
    else
      _get_line_length line (index + 1)
  in _get_line_length line 0

let (++) = Big_int.add_big_int
let (--) = Big_int.sub_big_int
let (~--) = Big_int.minus_big_int
let ( ** ) = Big_int.mult_big_int
let (%) = Big_int.mod_big_int
let (//) = Big_int.div_big_int
let ( >>= ) = Big_int.ge_big_int
let ( <<= ) = Big_int.le_big_int
let ( >> ) = Big_int.gt_big_int
let ( << ) = Big_int.lt_big_int
let to_big = Big_int.big_int_of_int
let of_big = Big_int.int_of_big_int
