open Str
open Utils
open Global
open Errors
open Warnings
open Imports
open Structures
open Getters
open Expressions
open Internationalisation.Translation
open Abstract_type_translation

let eval_expression_with_type str context =
  let expr = expr_of_string str in
  string_of_expr expr, type_of_expr context expr

let eval_expression str context =
  let expr, _ = eval_expression_with_type str context in expr

(*AUX for eval_code*)

let _is_if_scope = function
  | If _ -> true
  | _ -> false

let _increment_if = function
  | If i -> If (i + 1)
  | _ -> assert false

let _decrement_if = function
  | If i -> If (i - 1)
  | _ -> assert false

let _is_last_if = function
  | If 0 -> true
  | _ -> false

let _verify_type ret_expr var context =
  match var with
    | `Function(_, t) when t = type_of_expr context ret_expr -> ()
    | _ -> raise_type_error (get_string ReturnTypeMatchMessage) ~line: (get_line_no context.code context.index)

let rec _check_retcall ?(is_first = true) ret_expr context =
  match context.scopes with
    | [] -> raise_syntax_error (get_string UnexpectedReturn) ~line: (get_line_no context.code context.index)
    | Function (name,_) :: _ -> _verify_type ret_expr (StringMap.find name context.vars) context
    | (For | While) :: t -> if is_first then add_warning (get_string BreakingReturn) 0;
      _check_retcall ret_expr {context with scopes = t} ~is_first: false
    | _ :: t -> _check_retcall ret_expr {context with scopes = t} ~is_first

let rec get_fname_def_status scopes =
  match scopes with
    | [] -> false, ""
    | Function_definition name :: _ -> true, name
    | (Function (name,_))::_ -> false, name
    | _ :: r -> get_fname_def_status r

let _valid_pos context =
  let get_return_type name =
    match StringMap.find name context.vars with
      | `Function(_, t) -> t = `None
      | _ -> failwith "Illegal use"
  in
  match context.scopes with
  | (Function (name,_)) :: _ when not (get_return_type name) -> false
    | _-> true

(*Core*)

let rec eval_code context =
  let rec _eval_code context =
    (*print_string ("[+] Scopes: [" ^(str_of_scopes context.scopes)^"\n");*)
    let code = context.code and start_index = context.index in
    let depth = List.length context.scopes in
    let depth = format_depth depth context.scopes in
    let word, index = get_word code context.index in
    let context = {context with index = index} in
    match find_assoc word control_keywords with
    | Some (scope, func) ->
      let scopes =
        if word = "sinon" || word = "sinon_si" then
          if context.scopes <> [] && _is_if_scope (List.hd context.scopes) then
            _increment_if (List.hd context.scopes) :: List.tl context.scopes
          else
            raise_syntax_error ((get_string UnexpectedToken) ^ word ^ "'") ~line: (get_line_no code context.index)
        else
          scope :: context.scopes in
      let translation, context =
        (try
           func {context with index = (context.index - String.length word - 1); scopes}
         with Invalid_argument m as e ->
           if m = "index out of bounds" then
             raise_syntax_error ~line: (get_last_line context.code) (get_string UnexpectedEOF)
           else
             raise e) in
      let next_translation, context = _eval_code context
      in translation ^ next_translation, context
    | None ->
      let is_def, name = get_fname_def_status context.scopes in
      if word <> "debut" && word <> "variables" && is_def then
        raise_syntax_error ~line: (get_line_no code (start_index + 2)) (get_string ExpectedDebut)
      else match word with
        | "utiliser" -> eval_code (eval_utiliser context)
        | "variables" -> eval_code (eval_variables context)
        | "attributs" -> eval_code (eval_attributes ({context with scopes = (Attributes ""):: context.scopes}))
        | "methodes" ->
               if is_attr_declaration context.scopes then
                 let content  = try_update_err (get_line_no code context.index) (fun () -> _get_attrs_result context) in
                 let new_scopes = (Function_definition "") :: (Methods content) :: List.tl context.scopes in
                 let translated, context = eval_constructor {context with index = index ; scopes = new_scopes} in
                 let next, context = eval_code context in
                 translated^next, context
               else
                 raise_syntax_error "Cannot declare methods without arguments" ~line: (get_line_no context.code context.index)
        | "debut" -> if is_def then
            eval_code {context with scopes = (Function (name,false)):: List.tl context.scopes}
          else
            raise_syntax_error ~line: (get_line_no code start_index) (get_string UnexpectedDebut)
        | "retourner" -> let expr, i = get_line code context.index in
          let return_expression =
            if string_match (regexp "^[ \t]*instance[ \t]*$") expr 0 then
               ""
            else (
              let py_expr = try_update_err (get_line_no code context.index) (fun () -> eval_expression expr context) in
              _check_retcall (expr_of_string expr) context;
              try_update_warnings ~line: (get_line_no code start_index);
              "return " ^ py_expr)
          in
          let new_scopes = ret context.scopes name in
          let next, context = _eval_code {context with index = i; scopes = new_scopes} in
          get_indentation depth ^ return_expression ^ "\n" ^ next, context
        | "fin" ->
          if context.scopes = [] then
            raise_syntax_error (get_string UnexpectedFin) ~line: (get_line_no code context.index)
          else
            let last_scope = List.hd context.scopes in
            let index, scopes = if _is_if_scope last_scope && not (_is_last_if last_scope) then
                start_index, _decrement_if last_scope :: List.tl context.scopes
              else
                index, List.tl context.scopes
            in
            if (has_returned context.scopes name) then
              "", { context with index; scopes }
            else if (_valid_pos context ) then
              "", { context with index; scopes }
            else
              raise_syntax_error (get_string ExpectedReturn) ~line: (get_line_no code context.index)
        | "" -> if List.length context.scopes = 0 then
            "", context
          else
            raise_syntax_error "Unclosed scope: expected 'fin'" ~line: (get_line_no code context.index)
        | _ -> (* Expression or affectation *)
          let line_no = get_line_no code context.index in
          let r = regexp ("^[\n\t ]*\\([A-Za-z_][A-Za-z_0-9]*\\) *<- *\\(.*\\)\n") in
          if string_match r code start_index then   (* Affectation *)
            let end_index = match_end() in
            let var = matched_group 1 code
            and expr = matched_group 2 code in
            let var_type = try_update_err line_no (fun () -> get_var var context.vars)
            and expr, expr_type = try_update_err line_no (fun () -> eval_expression_with_type expr context) in
            if Type.is_compatible var_type expr_type then
              let next, context = _eval_code {context with index = end_index} in
              get_indentation depth ^ word ^ " = " ^ expr ^ "\n" ^ next, context
            else
              raise_unexpected_type_error_with_name var (Type.to_string var_type) (Type.to_string expr_type) ~line: (get_line_no code index)
          else
            let r =  regexp ("^[\n\t ]*instance +\\([A-Za-z_][A-Za-z_0-9]*\\) *<- *\\(.*\\)\n") in
            if string_match r code start_index then (*USE OF INSTANCE*)
                if is_class_context context.scopes then
                  let class_name = get_current_class_name context in
                  let end_index = match_end () in
                  let var = matched_group 1 code
                  and expr = matched_group 2 code in
                  let attr_meths, are_set = Type.get_attr_meths class_name context.vars in
                  let var_type = StringMap.find var attr_meths in
                  let expr, expr_type = try_update_err line_no (fun () -> eval_expression_with_type expr context) in
                  if Type.is_compatible var_type expr_type then
                    let are_set = StringMap.add var true are_set in
                    let vars = StringMap.add class_name (`Custom (class_name, attr_meths, are_set)) context.vars in
                    let context = {context with index = end_index; vars = vars} in
                    let next, context = _eval_code context in
                    get_indentation depth ^ "self." ^ var ^ " = " ^ expr ^ "\n" ^ next, context
                  else
                    raise_unexpected_type_error_with_name var (Type.to_string var_type) (Type.to_string expr_type) ~line: (get_line_no code index)
                else
                  raise_syntax_error "Keyword 'instance' cannot be used outside a class definition" ~line:(get_line_no code index)
            else
              let index = ignore_chrs code start_index in
              let line, index = get_line code index in
              let expr = try_update_err line_no (fun () -> eval_expression line context) in
              let next, context = _eval_code {context with index} in
              get_indentation depth ^ expr ^ "\n" ^ next, context
  in _eval_code context


(* Eval the files corresponding to the modules and import them in the python code *)
and eval_utiliser context =
  let write_pyfile filename =
    let naturl_name = filename ^ ".ntl" and py_name = filename ^ ".py" in
    if !import_mode = Overwrite || !import_mode = Moderated && not (Sys.file_exists py_name) then
      let code = translate_code (read_file naturl_name) in
      write_file py_name code
  in
  let line_no = get_line_no context.code context.index in
  let line, index = get_line context.code context.index in
  let dependencies = String.split_on_char ',' line in
  let vars = List.flatten (try_update_err line_no (fun () -> dependencies |> List.map get_imported_files_infos))
             |> List.map (function content, cwdir, namespace, filename, element ->
                 add_import namespace element;
                 Sys.chdir cwdir;
                 let imports_back = !Imports.imports in
                 let new_context = get_code_context content in Sys.chdir "..";
                 Imports.imports := imports_back;
                 write_pyfile filename;
                 let prefix = if element = None then namespace ^ "." else "" in
                 context.vars
                 |> StringMap.fold (fun key -> fun value -> fun map -> StringMap.add (prefix ^ key) value map) new_context.vars)
             |> List.fold_left (StringMap.union (fun _ -> fun _ -> fun t -> Some t)) context.vars
  in { context with index; vars }

(* This function only adds the new declared variables in the set *)
and eval_variables context =
  let {code; index; vars; scopes} = context in
  let rec eval_line vars type_struct = function
      [] -> vars
    | name :: t -> let name = String.trim name in
      eval_line (StringMap.add name type_struct vars) type_struct t
  in let rec _eval_variables vars code index =
       let line_no = get_line_no code index in
       let word, index = get_word code index
       in if word = "debut" then
         vars, index - 6
       else if word = "variables" then
         _eval_variables vars code index
       else if word = "fin" then
         raise_syntax_error (get_string UnexpectedFin) ~line: line_no
       else
         let line, index = get_line code index in
         let type_struct = try_update_err line_no (fun () -> Type.of_string word) in
         let vars = eval_line vars type_struct (String.split_on_char ',' line) in
         try_update_warnings ~line: line_no;
         _eval_variables vars code index
  in let vars, index = _eval_variables vars code index
  in { code; index; vars; scopes }

and eval_fonction context =
  let depth = List.length context.scopes - 1 in
  let depth = format_depth depth context.scopes in
  let line = get_line_no context.code context.index in
  (* A function is divided in a header (the name), parameters and a return type.
     This functions combine those parts *)
  let check_return_type i =
    let i = ignore_spaces context.code i in
    if context.code.[i] <> '-' then
      raise_syntax_error ((get_string UnexpectedChar) ^ (Char.escaped context.code.[i]) ^ (get_string InFunctionDefinition)) ~line: (get_line_no context.code i)
    else if context.code.[i + 1] <> '>' then
      raise_syntax_error ((get_string UnexpectedChar) ^ (Char.escaped context.code.[i + 1]) ^ (get_string InFunctionDefinition)) ~line: (get_line_no context.code (i + 1))
    else
      get_type context.code (i + 2)
  in
  let name, index = get_word context.code (context.index + 9) in (* 9 = 8 + 1 *)
  let prev_vars = context.vars in
  let names, index, vars, types = get_param context index in
  let index, type_ = check_return_type index in
  let fx = `Function (types, type_) in
  let prev_vars = StringMap.add name fx prev_vars in
  let vars = StringMap.add name fx vars in
  let cscopes = context.scopes in (*cscopes = current scopes*)
  try_update_warnings ~line;
  let next, context = eval_code {context with index; vars; scopes = set_fscope_name cscopes name} in
  let offset = if context.index >= String.length context.code - 1 then "" else "\n\n" in
  let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
  get_indentation depth ^ "def " ^ name ^ "(" ^ names ^ "):\n" ^ next ^ offset, {context with vars = prev_vars}

and eval_procedure context =
  let depth = List.length context.scopes - 1 in
  let depth = format_depth depth context.scopes in
  let line = get_line_no context.code context.index in
  (*Same logic as functions except that there is no need to check a return type*)
  let name, index = get_word context.code (context.index + 10) (*10 = 9 + 1*) in
  let prev_vars = context.vars in
  let names, index, vars, types = get_param context index in
  let fx = `Function (types, `None) in
  let prev_vars = StringMap.add name fx prev_vars in
  let vars = StringMap.add name fx vars in
  let cscopes = context.scopes in (*cscopes = current scopes*)
  try_update_warnings ~line;
  let next, context = eval_code {context with vars = vars; index = index; scopes = set_fscope_name cscopes name} in
  let offset = if context.index >= String.length context.code - 1 then "" else "\n\n" in
  let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
  get_indentation depth ^ "def " ^ name ^ "(" ^ names ^ "):\n" ^ next ^ offset, {context with vars = prev_vars}

and eval_si context =
  let code = context.code in
  let line = get_line_no code context.index in
  let depth = List.length context.scopes - 1 in
  let depth = format_depth depth context.scopes in
  (* This function checks that the expressions between 'si' and 'alors' is a boolean expression *)
  (* and returns "if <expression>:" followed by the rest of the code *)
  match get_word code context.index with
  | "si", i -> let expr, index = get_expression code i "alors" in
    let expr, type_struct = try_update_err line (fun () -> eval_expression_with_type expr context) in
    try_update_warnings ~line;
    if Type.is_compatible type_struct `Bool then
      if expr = "False" then
        let _, context = eval_code {context with index} in
        add_warning ~line (get_string AlwaysFalse) 3;
        "", context
      else
        begin
          if expr = "True" then
            add_warning ~line (get_string AlwaysTrue) 3;
          let next, context = eval_code {context with index}
          in let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next
          in get_indentation depth ^ "if " ^ expr ^ ":\n" ^ next, context
        end
    else
      raise_unexpected_type_error (Type.to_string `Bool) (Type.to_string type_struct) ~line
  | _ -> raise_syntax_error "si statement must start with 'si'" ~line

and eval_sinon_si context =
  let code = context.code in
  let line = get_line_no code context.index in
  let depth = List.length context.scopes - 1 in
  let depth = format_depth depth context.scopes in
  (* Same as si but sinon_si *)
  match get_word code context.index with
  | "sinon_si", i -> let expr, index = get_expression code i "alors" in
    let expr, type_struct = try_update_err line (fun () -> eval_expression_with_type expr context) in
    try_update_warnings ~line;
    if Type.is_compatible type_struct `Bool then
      if expr = "False" then
        let _, context = eval_code {context with index} in
        add_warning ~line (get_string AlwaysFalse) 3;
        "", context
      else
        begin
          if expr = "True" then
            add_warning ~line (get_string AlwaysTrue) 3;
          let next, context = eval_code {context with index}  in
          let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
          get_indentation depth ^ "elif " ^ expr ^ ":\n" ^ next, context
        end
    else
      raise_unexpected_type_error (Type.to_string `Bool) (Type.to_string `Bool) ~line
  | _ -> raise_syntax_error "sinon_si statement must start with 'sinon_si'" ~line

and eval_sinon context =
  let code = context.code in
  let line = get_line_no code context.index in
  let depth = List.length context.scopes - 1 in
  let depth = format_depth depth context.scopes in
  (* Replaces "sinon" by "else:\n"*)
  match get_word code context.index with
  | "sinon", i -> let next, context = eval_code {context with index = i} in
    let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
    get_indentation depth ^ "else:\n" ^ next, context
  | _ -> raise_syntax_error "sinon statement must start with 'sinon'" ~line

and eval_tant_que context =
  let code = context.code in
  let line = get_line_no code context.index in
  let depth = List.length context.scopes - 1 in
  let depth = format_depth depth context.scopes in
  (* Same as si but with 'tant_que' and 'faire' instead of 'si' and 'alors' *)
  match get_word code context.index with
  | "tant_que", i -> let expr, index = get_expression code i "faire" in
    let expr, type_struct = try_update_err line (fun () -> eval_expression_with_type expr context) in
    try_update_warnings ~line;
    let next, context = eval_code {context with index} in
    let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
    if Type.is_compatible type_struct `Bool then
      get_indentation depth ^ "while " ^ expr ^ ":\n" ^ next, context
    else
      raise_unexpected_type_error (Type.to_string `Bool) (Type.to_string type_struct) ~line
  | _ -> raise_syntax_error "tant_que loop must start with 'tant_que'" ~line

and eval_pour_chaque context =
  let code = context.code in
  let line = get_line_no code context.index in
  let depth = List.length context.scopes - 1 in
  let depth = format_depth depth context.scopes in
  (* A pour_chaque instruction has the form "pour_chaque <var> dans <iterable> faire"*)
  (* This function translates it to "for <var> in <iterable>)" *)
  let _, index = get_word code context.index in
  let var, index = get_expression code index "dans" in let var_type = try_update_err line (fun () -> get_var var context.vars) in
  let iterable, index = get_expression code index "faire"  in let iterable_expr, iterable_type = try_update_err line (fun () -> eval_expression_with_type iterable context) in
  (match Type.get_iterable_type iterable_type with
   | Some t -> if not (Type.is_compatible t var_type) then raise_unexpected_type_error (Type.to_string t) (Type.to_string var_type) ~line
   | None -> raise_type_error ((get_string TheType) ^ (Type.to_string iterable_type) ^ "' is not iterable") ~line);
  try_update_warnings ~line;
  let next, context = eval_code {context with index} in
  let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
  get_indentation depth ^ "for " ^ (String.trim var) ^ " in " ^ iterable_expr  ^ ":\n" ^ next, context
(*TODO Add translation for not iterable*)
and eval_pour context =
  let code = context.code in
  let line = get_line_no code context.index in
  let depth = List.length context.scopes - 1 in
  let depth = format_depth depth context.scopes in
  (* A pour instruction has the form "pour <var> de <start> a <end> faire"*)
  (* This function translates it to "for <var> in range(start, end + 1)" *)
  let _, index = get_word code context.index in
  let var, index = get_expression code index "de"         in let var_expr, var_type = try_update_err line (fun () -> eval_expression_with_type var context) in
  let start, index = get_expression code index "jusqu_a"  in let start_expr, start_type = try_update_err line (fun () -> eval_expression_with_type start context) in
  let end_, index = get_expression code index "faire"     in let end_expr, end_type = try_update_err line (fun () -> eval_expression_with_type (end_ ^ "+1") context) in
  try_update_warnings ~line;
  if Type.is_compatible var_type `Int && Type.is_compatible start_type `Int then
    let next, context = eval_code {context with index} in
    let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
    get_indentation depth ^ "for " ^ var_expr ^ " in range(" ^ start_expr ^ ", " ^ end_expr ^ "):\n" ^ next, context
  else
    raise_unexpected_type_error (Type.to_string `Int) (Type.to_string (find_bad_elt `None `Int [var_type; start_type; end_type])) ~line
(*POO related*)
and eval_type_definition context =
  let depth = List.length context.scopes -1 in
  let name, i = get_word context.code (ignore_spaces context.code (context.index + 13)) in
  let new_vars = StringMap.add name (`Custom (name, StringMap.empty, StringMap.empty)) context.vars in
  let scopes = List.tl context.scopes in
  let next, context = eval_code {context with index = i; vars = new_vars ; scopes = Class_def name :: scopes} in
  let next = if next = "" then get_indentation (depth+1)^"pass" else next in
  get_indentation depth ^"class "^name^":\n"^next^"\n", context

and eval_constructor context =
  let depth = List.length context.scopes - 2 in
  let line = get_line_no context.code context.index in
  let class_name = get_current_class_name context in
  let context = {context with index = (ignore_chrs context.code context.index)} in
  (* A function is divided in a header (the name), parameters and a return type.
     This functions combine those parts *)
  let check_return_type i =
    let i = ignore_spaces context.code i in
    if context.code.[i] <> '-' then
      raise_syntax_error ((get_string UnexpectedChar) ^ (Char.escaped context.code.[i]) ^ (get_string InFunctionDefinition)) ~line: (get_line_no context.code i)
    else if context.code.[i + 1] <> '>' then
      raise_syntax_error ((get_string UnexpectedChar) ^ (Char.escaped context.code.[i + 1]) ^ (get_string InFunctionDefinition)) ~line: (get_line_no context.code (i + 1))
    else
      let result, i = get_word context.code (i+2) in
      if result = class_name then
        i
      else
        raise_syntax_error "The constructor does not return the right type." ~line: line
  in
  let name, index = get_word context.code (context.index + 9) in (* 9 = 8 + 1 *)
  let name = if name <> "nouveau" then raise_syntax_error ("The first method needs to be a constructor but got: "^name) else "__init__" in
  let prev_vars = context.vars in
  let names, index, vars, types = get_param context index in
  let index = check_return_type index in
  let attr_meths, are_set = Type.get_attr_meths class_name context.vars in
  let fx = `Function (types, `Custom (class_name, attr_meths, are_set)) in
  let prev_vars = StringMap.add name fx prev_vars in
  let vars = StringMap.add name fx vars in
  let cscopes = context.scopes in (*cscopes = current scopes*)
  try_update_warnings ~line;
  let next, context = eval_code {context with index; vars; scopes = set_fscope_name cscopes name} in
  let offset = if context.index >= String.length context.code - 1 then "" else "\n\n" in
  let next = if string_match (regexp "^ *\n") next 0 then get_indentation (depth + 1) ^ "pass\n" else next in
  let next = (get_methods_content context.scopes) ^ next in
  let attr_meths, are_set = Type.get_attr_meths class_name context.vars in
  let final_class_type = `Custom (class_name, attr_meths, are_set) in
  let prev_vars = StringMap.add class_name final_class_type prev_vars in
  let prev_vars = StringMap.add name (`Function (types, final_class_type)) prev_vars in
  get_indentation depth ^ "def " ^ name ^ "(" ^ names ^ "):\n" ^ next ^ offset, {context with vars = prev_vars}

and control_keywords =
  [
    "fonction", (Function_definition "", eval_fonction);
    "procedure", (Function_definition "", eval_procedure);
    "si", (If 0, eval_si);
    "sinon", (If 0, eval_sinon);
    "sinon_si", (If 0, eval_sinon_si);
    "pour", (For, eval_pour);
    "pour_chaque", (For, eval_pour_chaque);
    "tant_que", (While, eval_tant_que) ;
    "type_abstrait", (Class_def "", eval_type_definition)
  ]


and get_code_context code =
  let code = String.trim code and index = 0 and vars = StringMap.empty and scopes = [] in
  let _, context = try_catch stderr (fun () -> eval_code {code; index; vars; scopes}) in
  context

and translate_code code =
  let code = String.trim code and index = 0 and vars = StringMap.empty and scopes = [] in
  let translation, _ = try_catch stderr (fun () -> eval_code {code; index; vars; scopes}) in
  let translation = String.trim translation in
  let translation = if not (are_imports_empty ()) && let word, _ = get_word translation 0 in word = "def"
    then "\n" ^ translation
    else translation
  in get_imports () ^ (if are_imports_empty () then "" else "\n") ^ translation
