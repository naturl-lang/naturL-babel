open Utils
open Str
open Errors
open Structures
open Getters
open Expressions
open Internationalisation.Translation

(*AUX*)
(*CORE*)
(*The scope Attributes and Methodes will have as parameter the string of the translated default variables*)
let eval_attributes context =
  let depth = List.length context.scopes in
  let rec manage_line length line i context class_name result =
    if i >= length then
      context, ""
    else
      let word, i = get_word line (ignore_chrs line i) in
      if word = "<-" then
        let expr, _ = get_word line (ignore_chrs line i) in
        let name, _ = get_word result 5 in
        let expr = expr_of_string context expr in
        let attr_meths, are_set = Type.get_attr_meths class_name context.vars  in
        if (StringMap.find name attr_meths) = type_of_expr {context with vars=attr_meths} expr then
          let expr = string_of_expr context expr in
          let result = get_indentation depth ^ result ^ " = " ^ expr ^"\n" in
          let are_set = StringMap.add name true are_set in
          let vars = context.vars |> StringMap.add class_name (`Class (attr_meths, are_set)) in
          { context with vars }, result
        else
          raise_type_error (get_string GivenExpression) ~line: (get_line_no context.code context.index)
      else
        let type_ = Type.of_string context.vars word in
        (*getting var name*)
        let name, i = get_word line (ignore_chrs line i) in
        let attr_meths, are_set = Type.get_attr_meths class_name context.vars in
        let attr_meths = StringMap.add name type_ attr_meths in
        let are_set = StringMap.add name false are_set in
        let result = "self." ^ name in
        let defs = context.defs |> StringMap.add (class_name ^ "." ^ name) {
            line = get_line_no context.code context.index;
            filename = context.filename;
            scopes = context.scopes
          }
        and vars = context.vars |> StringMap.add class_name (`Class (attr_meths, are_set)) in
        manage_line length line i { context with vars; defs } class_name result
  in
  let rec _main_process context result code_len =
    let whitespace = "[ \t]*" in
    if context.index < code_len then
      let line, i = get_line context.code context.index in
      if string_match (regexp (whitespace ^ "methodes" ^ whitespace)) line 0 then
        {context with scopes = (Attributes result):: List.tl context.scopes}
      else if string_match (regexp (whitespace ^ "fin" ^ whitespace)) line 0 then
        {context with scopes = (Attributes result) :: List.tl context.scopes}
      else
        let context, l_result =  manage_line (String.length line) line 0 context (get_current_class_name context) "" in
        _main_process {context with index = i } (result ^ l_result) code_len
    else
      raise_syntax_error (get_string AttributeScope) ~line: (get_line_no context.code context.index)
  in
  let code = context.code in
  try_update_err (get_line_no code context.index) (fun () -> _main_process context "" (String.length code))

and is_attr_declaration scopes =
  match List.hd scopes with
  | Attributes _ -> true
  | _ -> false

let rec is_class_context scopes =
  match scopes with
  | [] -> false
  | Class_def _ :: _-> true
  | _ :: r -> is_class_context r

let rec is_method_context scopes =
  match scopes with
    [] -> false
  | Class_def _ :: _ -> true
  | _ :: t -> is_method_context t

let _get_attrs_result context =
  match List.hd context.scopes with
  | Attributes content -> content
  | _ -> raise_syntax_error (get_string DeclareAttributes) ~line: (get_line_no context.code context.index)

let rec get_methods_content scopes =
  match scopes with
    [] -> failwith "Invalid use of get_methods_content, no current Methods scope"
  | Methods content :: _ -> content
  | _ :: r -> get_methods_content r

let rec format_depth depth scopes =
  match scopes with
  | Methods _ :: _ -> depth - 1
  | [] -> depth
  | _ ::r -> format_depth depth r

(* Adds a field to the attributes of the given class *)
let add_class_attr class_name attr_name attr is_set vars =
  let attr_meths, are_set = Type.get_attr_meths class_name vars in
  let attr_meths = attr_meths |> StringMap.add attr_name attr in
  let are_set = are_set |> StringMap.add attr_name is_set in
  vars |> StringMap.add class_name (`Class (attr_meths, are_set))
