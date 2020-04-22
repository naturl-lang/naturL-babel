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

let eval_expression_with_type str vars =
  let expr = expr_of_string str in
  string_of_expr expr, type_of_expr vars expr

let eval_expression str vars =
  let expr, _ = eval_expression_with_type str vars in expr

(*AUX for eval_code*)
let _verify_type ret_expr var context =
  match var with
    | `Function(_, t) when t = type_of_expr context.vars ret_expr -> ()
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
    (if !Global.debug then
    print_string ("[+] Scopes: [" ^(str_of_scopes context.scopes)^"\n")
    else  () );
    let code = context.code and start_index = context.index in
    let depth = List.length context.scopes in
    let word, index = get_word code context.index in
    let context = {context with index = index} in
    match find_assoc word control_keywords with
    | Some (scope, func) ->
      let scopes =
        if word = "sinon" || word = "sinon_si" then
          if context.scopes <> [] && List.hd context.scopes = If then
            scope :: List.tl context.scopes
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
        | "utiliser" -> _eval_code (eval_utiliser context)
        | "variables" -> _eval_code (eval_variables context)
        | "debut" -> if is_def then
            eval_code {context with scopes = (Function (name,false)):: List.tl context.scopes}
          else
            raise_syntax_error ~line: (get_line_no code start_index) (get_string UnexpectedDebut)
        | "retourner" -> let expr, i = get_line code context.index in
          let py_expr = try_update_err (get_line_no code context.index) (fun () -> eval_expression expr context.vars) in
          _check_retcall (expr_of_string expr) context;
          try_update_warnings ~line: (get_line_no code start_index);
          let new_scopes = ret context.scopes name in
          let next, context = _eval_code {context with index = i; scopes = new_scopes} in
          get_indentation depth ^ "return " ^ py_expr ^ "\n" ^ next, context
        | "fin" -> if (has_returned context.scopes name) then
            "", {context with scopes = List.tl context.scopes}
          else if context.scopes = [] then
            raise_syntax_error (get_string UnexpectedFin) ~line: (get_line_no code context.index)
          else if (_valid_pos context ) then
            "", {context with scopes = List.tl context.scopes}
          else
            raise_syntax_error (get_string ExpectedReturn) ~line: (get_line_no code context.index)
        | "" -> if List.length context.scopes = 0 then "", context else raise_syntax_error (get_string ExpectedFin) ~line: (get_line_no code context.index)
        | _ -> (* Expression or affectation *)
          let line_no = get_line_no code context.index in
          let r = regexp ("^[\n\t ]*\\([A-Za-z_][A-Za-z_0-9]*\\) *<- *\\(.*\\)\n") in
          if string_match r code start_index then   (* Affectation *)
            let end_index = match_end() in
            let var = matched_group 1 code
            and expr = matched_group 2 code in
            let var_type = try_update_err line_no (fun () -> get_var var context.vars)
            and expr, expr_type = try_update_err line_no (fun () -> eval_expression_with_type expr context.vars) in
            if Type.is_compatible var_type expr_type then
              let next, context = _eval_code {context with index = end_index} in
              get_indentation depth ^ word ^ " = " ^ expr ^ "\n" ^ next, context
            else
              raise_unexpected_type_error_with_name var (Type.to_string var_type) (Type.to_string expr_type) ~line: (get_line_no code index)
          else (* Expression, e.g: function call *)
            let index = ignore_chrs code start_index in
            let line, index = get_line code index in
            let expr = try_update_err line_no (fun () -> eval_expression line context.vars) in
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
                 add_import namespace element; write_pyfile filename;
                 Sys.chdir cwdir;
                 let new_context = get_code_context content in Sys.chdir "..";
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
  in {code; index; vars; scopes}

and eval_fonction context =
  let depth = List.length context.scopes - 1 in
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
  let names, index, vars, types = get_param context.vars context.code index in
  let index, type_ = check_return_type index in
  let fx = `Function (types, type_) in
  let prev_vars = StringMap.add name fx prev_vars in
  let vars = StringMap.add name fx vars in
  let cscopes = context.scopes in (*cscopes = current scopes*)
  try_update_warnings ~line;
  let next, context = eval_code {context with index = index; vars = vars; scopes = set_fscope_name cscopes name} in
  let offset = if context.index >= String.length context.code - 1 then "" else "\n\n" in
  let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
  get_indentation depth ^ "def " ^ name ^ "(" ^ names ^ "):\n" ^ next ^ offset, {context with vars = prev_vars}

and eval_procedure context =
  let depth = List.length context.scopes - 1 in
  let line = get_line_no context.code context.index in
  (*Same logic as functions except that there is no need to check a return type*)
  let name, index = get_word context.code (context.index + 10) (*10 = 9 + 1*) in
  let prev_vars = context.vars in
  let names, index, vars, types = get_param context.vars context.code index in
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
  (* This function checks that the expressions between 'si' and 'alors' is a boolean expression *)
  (* and returns "if <expression>:" followed by the rest of the code *)
  match get_word code context.index with
  | "si", i -> let expr, index = get_expression code i "alors" in
    let expr, type_struct = try_update_err line (fun () -> eval_expression_with_type expr context.vars) in
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
  (* Same as si but sinon_si *)
  match get_word code context.index with
  | "sinon_si", i -> let expr, index = get_expression code i "alors" in
    let expr, type_struct = try_update_err line (fun () -> eval_expression_with_type expr context.vars) in
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
  (* Same as si but with 'tant_que' and 'faire' instead of 'si' and 'alors' *)
  match get_word code context.index with
  | "tant_que", i -> let expr, index = get_expression code i "faire" in
    let expr, type_struct = try_update_err line (fun () -> eval_expression_with_type expr context.vars) in
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
  (* A pour_chaque instruction has the form "pour_chaque <var> dans <iterable> faire"*)
  (* This function translates it to "for <var> in <iterable>)" *)
  let _, index = get_word code context.index in
  let var, index = get_expression code index "dans" in let var_type = try_update_err line (fun () -> get_var var context.vars) in
  let iterable, index = get_expression code index "faire"  in let iterable_expr, iterable_type = try_update_err line (fun () -> eval_expression_with_type iterable context.vars) in
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
  (* A pour instruction has the form "pour <var> de <start> a <end> faire"*)
  (* This function translates it to "for <var> in range(start, end + 1)" *)
  let _, index = get_word code context.index in
  let var, index = get_expression code index "de"         in let var_expr, var_type = try_update_err line (fun () -> eval_expression_with_type var context.vars) in
  let start, index = get_expression code index "jusqu_a"  in let start_expr, start_type = try_update_err line (fun () -> eval_expression_with_type start context.vars) in
  let end_, index = get_expression code index "faire"     in let end_expr, end_type = try_update_err line (fun () -> eval_expression_with_type (end_ ^ "+1") context.vars) in
  try_update_warnings ~line;
  if Type.is_compatible var_type `Int && Type.is_compatible start_type `Int then
    let next, context = eval_code {context with index} in
    let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
    get_indentation depth ^ "for " ^ var_expr ^ " in range(" ^ start_expr ^ ", " ^ end_expr ^ "):\n" ^ next, context
  else
    raise_unexpected_type_error (Type.to_string `Int) (Type.to_string (find_bad_elt `None `Int [var_type; start_type; end_type])) ~line

and control_keywords =
  [
    "fonction", (Function_definition "", eval_fonction);
    "procedure", (Function_definition "", eval_procedure);
    "si", (If, eval_si);
    "sinon", (Else, eval_sinon);
    "sinon_si", (If, eval_sinon_si);
    "pour", (For, eval_pour);
    "pour_chaque", (For, eval_pour_chaque);
    "tant_que", (While, eval_tant_que)
  ]


and get_code_context code =
  let code = String.trim code and index = 0 and vars = StringMap.empty and scopes = [] in
  let _, context = try_catch stderr (fun () -> eval_code {code; index; vars; scopes})
  in context

and translate_code code =
  let code = String.trim code and index = 0 and vars = StringMap.empty and scopes = [] in
  let translation, _ = try_catch stderr (fun () -> eval_code {code; index; vars; scopes}) in
  let translation = String.trim translation in
  let translation = if not (are_imports_empty ()) && let word, _ = get_word translation 0 in word = "def"
    then "\n" ^ translation
    else translation
  in get_imports () ^ (if are_imports_empty () then "" else "\n") ^ translation
