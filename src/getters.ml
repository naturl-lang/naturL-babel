open Str
open Syntax
open Utils
open Global
open Structures
open Errors
open Internationalisation.Translation

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
    raise_syntax_error ~line: (get_last_line code) (get_string UnexpectedEOF)

let rec ignore_spaces code i =
  if i < (String.length code) then
    if code.[i] = ' ' then
      ignore_spaces code (i + 1)
    else
      i
  else
    raise_syntax_error ~line: (get_last_line code) (get_string UnexpectedEOF)

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
    raise_syntax_error ~line: (get_line_no code index) ((get_string MissingKeyword) ^ terminator ^ "'")

let get_line code i =
  let len = String.length code in
  let rec _get_line escaped i line =
    if i < len then
      match code.[i] with
        '\n' -> if escaped then _get_line false (i + 1) line else line, i + 1
      | '|' -> _get_line true (i + 1) line
      | x -> if escaped && not (List.mem x [' '; '\t']) then
          raise_syntax_error ((get_string UnexpectedChar) ^ (String.make 1 x) ^ (get_string AfterLineContinuation))
        else
          _get_line escaped (i + 1) (line ^ (String.make 1 x))
    else
      line, len
  in _get_line false i ""

(*Check if the specified type is a valid builtin or user-defined type, if that is the case, returns the right type *)
let get_type vars code index =
  let t, i = get_word code index in
  i, try_update_err (get_line_no code index) (fun () -> Type.of_string vars t)

(*Gets the parameters of the function or the procedure*)
let get_param context index =
  let set_names names =
    match context.scopes with
      | Function_definition _ :: Methods _ :: _ when names = "" -> "self"
      | Function_definition _ :: Methods _ :: _ -> "self, " ^ names
      | _ -> names
  in
  let rec _get_params ?(is_first = false) vars names index ?(types = []) = function
    | [] -> set_names names, index, vars, List.rev types
    | h :: t -> let r = regexp {|^\(.*\)\ +\([a-zA-Z_][a-zA-Z_0-9]*\)$|} in
      let _ = string_match r h 0 in
      let type_ = Type.of_string vars (matched_group 1 h) in
      let name = matched_group 2 h in
      let i = match_end () in
      let new_name = resolve_py_conficts name in
      let sep = if is_first then "" else ", " in
      _get_params (StringMap.add (String.trim name) type_ vars) (names ^ sep ^ new_name) (index + i + 1) ~types: (type_ :: types) t in
  let r = regexp {| *\(.+ [A-Za-z_][A-Za-z0-9_]*\(, ?[^ ]+ [A-Za-z_][A-Za-z0-9_]*\)*\))|} in
  if string_match r context.code index then
    _get_params context.vars "" (index + 1) (split (regexp ",") (matched_group 1 context.code)) ~is_first: true
  else if string_match (regexp {|\( *)\)|}) context.code index then
    set_names "", match_end(), context.vars, []
  else
      raise_syntax_error ~line: (get_line_no context.code index) (get_string InvalidFunctionDefinition)

let get_var name ?main_vars vars =
  let main_vars = Option.value main_vars ~default: vars in
  let name = String.trim name
  and r = regexp {|\([a-zA-Z_][a-zA-Z_0-9]*\)\(\.[a-zA-Z_][a-zA-Z_0-9]*\)+|} in
  let name, vars = if string_match r name 0 then
      let var_name = matched_group 1 name in
      let attr_name = matched_group 2 name in
      let attr_name = String.sub attr_name 1 (String.length attr_name - 1) in
      match StringMap.find_opt var_name vars with
      | Some `Custom class_name -> (match StringMap.find_opt class_name main_vars with
          | Some `Class (attrs, _) -> attr_name, attrs
          | None -> attr_name, vars   (* We are already inside the class *)
          | _ -> assert false)
      | Some t -> raise_name_error ((get_string TheType) ^ (Type.to_string t) ^ (get_string HasNoAttribute) ^ attr_name)
      | None -> raise_name_error (get_string UnknownVariable ^ var_name ^ "'")
    else
      name, vars
  in
  try StringMap.find name vars
  with Not_found -> raise_name_error ((get_string UnknownVariable) ^ name ^ "'")


(* Returns information about the files that need to be imported : *)
(* A list of tuples (file_content, cwdir, namespace, name, element) *)
(* where cwdir is the absolute path of the file's parent directory *)
(* and element is an option indicating the imported element *)
let rec get_imported_files_infos ?(prefix = "") ?(element = None) name =
  (* Generate an __init__.py file corresponding to the given path *)
  let generate__init__py () =
    if !import_mode = Overwrite || !import_mode = Moderated && not (Sys.file_exists "__init__.py") then
      let content = ref "" in
      Sys.readdir "." |> Array.iter (function file ->
          if Sys.is_directory file then
            content := !content ^ "from ." ^ file ^ " import *\n"
          else if Filename.extension file = ".ntl" then
            content := !content ^ "from ." ^ (Filename.remove_extension file) ^ " import *\n");
      write_file "__init__.py" !content
  in
  let id_reg = "[a-zA-Z_][a-zA-Z0-9_]*" in
  let r = "\\(" ^ id_reg ^ "\\)\\(\\(\\." ^ id_reg ^ "\\)" in
  if Str.string_match (Str.regexp (r ^ "+\\)$")) name 0 then  (* pack.mod *)
    let dir =  Str.matched_group 1 name in
    let name = Str.matched_group 2 name in
    let name = String.sub name 1 (String.length name - 1) in
    if Sys.file_exists dir && Sys.is_directory dir then
      begin
        if dir = "std" && naturL_path <> None then
          Sys.chdir (Filename.concat (Option.get naturL_path) "std")
        else
          Sys.chdir dir;
        let infos = get_imported_files_infos name ~prefix: (prefix ^ dir ^ ".")  ~element  in
        Sys.chdir "..";
        infos
      end
    else if dir = "std" && naturL_path <> None then
      begin
        Sys.chdir (Filename.concat (Option.get naturL_path) "std");
        let infos = get_imported_files_infos name ~prefix: (prefix ^ dir ^ ".")  ~element  in
        Sys.chdir "..";
        infos
      end
    else
      raise_import_error ((get_string UnknownPackage) ^ dir ^ "'")
  else if Str.string_match (Str.regexp (r ^ "*\\)\\.\\*$")) name 0 then (* pack.* *)
    let name = matched_group 1 name ^ (matched_group 2 name) in
    get_imported_files_infos ~element: (Some "*") name
  else if Sys.file_exists (name ^ ".ntl") && not (Sys.is_directory (name ^ ".ntl")) then (* mod *)
    let content = read_file (name ^ ".ntl")
    and cwdir = Sys.getcwd ()
    and namespace = prefix ^ name
    and filename = Filename.concat (Sys.getcwd ()) name in
    [content, cwdir, namespace, filename, element]
  else if Sys.file_exists name && Sys.is_directory name || (name = "std" && naturL_path <> None) then
    let naturl_package = Filename.concat name "naturl-package" in
    if name = "std" && naturL_path <> None then Sys.chdir (Option.get naturL_path);
    if Sys.file_exists naturl_package && not (Sys.is_directory naturl_package) then
      let dir = name in
      let namespace = prefix ^ dir in
      Sys.chdir dir;
      generate__init__py ();
      let imports = ref [] in
      let infos = read_lines "naturl-package"
                  |> List.filter (fun name -> String.trim name <> "")
                  |> List.map (fun name ->
                      get_imported_files_infos name ~prefix: (prefix ^ dir ^ ".") ~element
                      |> List.map (function content, cwdir, _, filename, element ->
                          imports := namespace :: !imports;content, cwdir, namespace, filename, element))
                  |> List.concat in
      Sys.chdir "..";
      infos
    else
      raise_import_error ((get_string CannotImportPackage) ^ prefix ^ name ^ (get_string MissingNaturlPackage))
  else
    raise_import_error ((get_string UnknownPackage) ^ prefix ^ name ^ "'")
