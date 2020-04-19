open Str
open Utils
open Structures
open Errors

(* Returns the line number corresponding to the current index in the code *)
let rec get_line_no code index =
  if index < 0 then
    1
  else
    (if index < String.length code && code.[index] = '\n' then 1 else 0) + get_line_no code (index - 1)

let get_last_line code =
  get_line_no code (String.length code - 1)

let rec ignore_chrs code i =
  if i < (String.length code) then
    match code.[i] with
      ' ' | '\n' | '\t' | '\r' | '(' | ')' | ',' -> ignore_chrs code (i + 1)
    | _-> i
  else
    raise_syntax_error ~line: (get_last_line code) "Unexpected EOF while parsing"

let rec ignore_spaces code i =
  if i < (String.length code) then
    if code.[i] = ' ' then
      ignore_spaces code (i + 1)
    else
      i
  else
    raise_syntax_error ~line: (get_last_line code) "Unexpected EOF while parsing"

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
  let rec _get_params ?(is_first = false) vars names index ?(types = []) = function
    | [] -> names, index, vars, List.rev types
    | h :: t -> let i, type_ = get_type h 0 in
      let name, i = get_word h i in
      let sep = if is_first then "" else ", " in
      _get_params (StringMap.add (String.trim name) type_ vars) (names ^ sep ^ name) (index + i + 1) ~types: (type_ :: types) t in
  let r = regexp {|\(\([a-z_]+\|\?\) [A-Za-z_][A-Za-z0-9_]*\(, ?\([a-z_]\|\?\)+ [A-Za-z_][A-Za-z0-9_]*\)*\))|} in
  if string_match r code index then
    _get_params vars "" (index + 1) (split (regexp ",") (matched_group 1 code)) ~is_first: true
  else if string_match (regexp {|\( *)\)|}) code index then
    "", match_end(), vars, []
  else
    raise_syntax_error ~line: (get_line_no code index) "Invalid function definition"

let get_var name vars =
  let name = String.trim name in
  try StringMap.find name vars
  with Not_found -> raise_name_error ("Unknown variable " ^ name)


(* Returns information about the files that need to be imported : *)
(* A list of tuples (file_content, cwdir, namespace, infos) *)
(* where cwdir is the absolute path of the file's parent directory *)
let rec get_imported_files_infos ?(prefix = "") name =
  let id_reg = "[a-zA-Z_][a-zA-Z0-9_]*" in
  let r = regexp ("\\(" ^ id_reg ^ "\\)\\(\\(\\." ^ id_reg ^ "\\)+\\)") in
  if Str.string_match r name 0 then
    let dir =  Str.matched_group 1 name in
    if Sys.file_exists dir && Sys.is_directory dir then
      let name = Str.matched_group 2 name in
      let name = String.sub name 1 (String.length name - 1) in
      Sys.chdir dir;
      let infos = get_imported_files_infos name ~prefix: (prefix ^ dir ^ ".") in
      Sys.chdir "..";
      infos
    else
      raise_import_error ("Unknown package '" ^ dir ^ "'")
  else if Sys.file_exists (name ^ ".ntl") && not (Sys.is_directory (name ^ ".ntl")) then
    let content = read_file (name ^ ".ntl") in
    let cwdir = Sys.getcwd () in
    let namespace = prefix ^ name in
    [content, cwdir, namespace, name]
  else if Sys.file_exists name && Sys.is_directory name then
    let naturl_package = name ^ "/naturl-package" in
    if Sys.file_exists naturl_package && not (Sys.is_directory naturl_package) then
      let dir = name in
      let namespace = prefix ^ dir in
      Sys.chdir dir;
      let infos = read_lines "naturl-package"
                  |> List.map (fun name ->
                      get_imported_files_infos name ~prefix: (prefix ^ dir ^ ".")
                      |> List.map (function content, cwdir, _, name -> content, cwdir, namespace, name))
                |> List.concat in
      Sys.chdir "..";
      infos
    else
      raise_import_error ("Can not import package '" ^ prefix ^ name ^ "' (missing naturl-package file)")
  else
    raise_import_error ("Unknown module or package '" ^ prefix ^ name ^ "'")
