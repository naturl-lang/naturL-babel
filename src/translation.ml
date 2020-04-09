open Str
open Utils
open Errors
open Structures
open Getters
open Expressions


let eval_expression_with_type str vars =
  let expr = expr_of_string str in
  string_of_expr expr, type_of_expr vars expr

let eval_expression str vars =
  let expr, _ = eval_expression_with_type str vars in expr

let rec eval_code context =
  let rec _eval_code context =
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
            raise_syntax_error ("Unexpected token '" ^ word ^ "'") ~line: (get_line_no code context.index)
        else
          scope :: context.scopes in
      let translation, context =
        (try
           func {context with index = (context.index - String.length word - 1); scopes}
         with Invalid_argument m as e ->
           if m = "index out of bounds" then
             raise_syntax_error ~line: (get_last_line context.code) "Unexpected EOF"
           else
             raise e) in
      let next_translation, context = _eval_code context
      in translation ^ next_translation, context
    | None ->
      match word with
      | "variables" -> _eval_code (eval_variables context)
      | "debut" -> eval_code context
      | "retourner" -> let expr, i = get_line code context.index in
        let expr = try_update_err (get_line_no code context.index) (fun () -> eval_expression expr context.vars) in
        let next, context = _eval_code {context with index = i} in
        get_indentation depth ^ "return " ^ expr ^ "\n" ^ next, context
      | "fin" -> if context.scopes <> [] then
          "", {context with scopes = List.tl context.scopes}
        else
          raise_syntax_error "Unexpected token 'fin'" ~line: (get_line_no code context.index)
      | "" -> if List.length context.scopes = 0 then "", context else raise_syntax_error "Unclosed scope: expected 'fin'" ~line: (get_line_no code context.index + 1)
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
          let expr, index = get_line code (index - 1) in
          let expr = try_update_err line_no (fun () -> eval_expression (word ^ expr) context.vars) in
          let next, context = _eval_code {context with index} in
          get_indentation depth ^ expr ^ "\n" ^ next, context
  in _eval_code context


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
         raise_syntax_error "Unexpected token 'fin'" ~line: line_no
       else
         let line, index = get_line code index in
         let type_struct = try_update_err line_no (fun () -> Type.of_string word) in
         let vars = eval_line vars type_struct (String.split_on_char ',' line) in
         _eval_variables vars code index
  in let vars, index = _eval_variables vars code index
  in {code; index; vars; scopes}

and eval_fonction context =
  let depth = List.length context.scopes - 1 in
  (* A function is divided in a header (the name), parameters and a return type.
    This functions combine those parts *)
  let check_return_type i =
    let i = ignore_spaces context.code i in
    if context.code.[i] <> '-' then
      raise_syntax_error ("Unexpected character '" ^ (String.make 1 context.code.[i]) ^ "' in function definition") ~line: (get_line_no context.code i)
    else if context.code.[i + 1] <> '>' then
      raise_syntax_error ("Unexpected character '" ^ (String.make 1 context.code.[i + 1]) ^ "' in function definition") ~line: (get_line_no context.code (i + 1))
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
  let next, context = eval_code {context with index = index; vars = vars} in
  let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
  get_indentation depth ^ "def " ^ name ^ "(" ^ names ^ "):\n" ^ next, {context with vars = prev_vars}

and eval_procedure context =
  let depth = List.length context.scopes - 1 in
  (*Same logic as functions except that there is no need to check a return type*)
  let name, index = get_word context.code (context.index + 10) (*10 = 9 + 1*) in
  let prev_vars = context.vars in
  let names, index, vars, types = get_param context.vars context.code index in
  let fx = `Function (types, `None) in
  let prev_vars = StringMap.add name fx prev_vars in
  let vars = StringMap.add name fx vars in
  let next, context = eval_code {context with vars = vars; index = index} in
  let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
  get_indentation depth ^ "def " ^ name ^ "(" ^ names ^ "):\n" ^ next, {context with vars = prev_vars}

and eval_si context =
  let code = context.code in
  let line = get_line_no code context.index in
  let depth = List.length context.scopes - 1 in
  (* This function checks that the expressions between 'si' and 'alors' is a boolean expression *)
  (* and returns "if <expression>:" followed by the rest of the code *)
  match get_word code context.index with
  | "si", i -> let expr, index = get_expression code i "alors" in
    let expr, type_struct = try_update_err line (fun () -> eval_expression_with_type expr context.vars) in
    if Type.is_compatible type_struct `Bool then
      let next, context = eval_code {context with index}
      in let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next
      in get_indentation depth ^ "if " ^ expr ^ ":\n" ^ next, context
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
    if Type.is_compatible type_struct `Bool then
      if expr = "False" then
        let _, context = eval_code {context with index} in
        "", context
      else
        let next, context = eval_code {context with index}  in
        let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
        get_indentation depth ^ "elif " ^ expr ^ ":\n" ^ next, context
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
  let var, index = get_expression code index "dans"        in let var_type = try_update_err line (fun () -> get_var var context.vars) in
  let iterable, index = get_expression code index "faire"  in let iterable_expr, iterable_type = try_update_err line (fun () -> eval_expression_with_type iterable context.vars) in
  (match Type.get_iterable_type iterable_type with
   | Some t -> if not (Type.is_compatible t var_type) then raise_unexpected_type_error (Type.to_string t) (Type.to_string var_type) ~line
   | None -> raise_type_error ("Type '" ^ (Type.to_string iterable_type) ^ "' is not iterable") ~line);
  let next, context = eval_code {context with index} in
  let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
  get_indentation depth ^ "for " ^ (String.trim var) ^ " in " ^ iterable_expr  ^ ":\n" ^ next, context

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
  if Type.is_compatible var_type `Int && Type.is_compatible start_type `Int then
    let next, context = eval_code {context with index} in
    let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
    get_indentation depth ^ "for " ^ var_expr ^ " in range(" ^ start_expr ^ ", " ^ end_expr ^ "):\n" ^ next, context
  else
    raise_unexpected_type_error (Type.to_string `Int) (Type.to_string (find_bad_elt `None `Int [var_type; start_type; end_type])) ~line

and control_keywords =
  [
    "fonction", (Function, eval_fonction);
    "procedure", (Function, eval_procedure);
    "si", (If, eval_si);
    "sinon", (Else, eval_sinon);
    "sinon_si", (If, eval_sinon_si);
    "pour", (For, eval_pour);
    "pour_chaque", (For, eval_pour_chaque);
    "tant_que", (While, eval_tant_que)
  ]

let translate_code code =
  let code = String.trim code and index = 0 and vars = StringMap.empty and scopes = [] in
  let translation, _ = try_catch stderr (fun () -> eval_code {code; index; vars; scopes})
  in String.trim translation
