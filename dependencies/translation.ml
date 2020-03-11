open Str
open Utils
open Errors
open Structures
open Getters
open Expressions


let eval_expression_with_type oc str vars =
  let separators = "\\([\t() ]*\\)"
  and replacements = [
    "et", "and";
    "ou", "or";
    "non", "not";
    "=", "==";
    ">==", ">=";
    "<==", "<=";
    "vrai", "True";
    "faux", "False"
  ] in
  let rec _replace str = function
    | [] -> str
    | (prev, new_word) :: t ->
      let r = regexp (separators ^ prev ^ separators)
      in _replace (global_replace r ("\\1" ^ new_word ^ "\\2") str) t
  in
  let is_valid, type_struct = check_expr oc str vars
  in if is_valid then
    String.trim (_replace str replacements), type_struct
  else
    semantic_error oc ("Inconsistent expression '" ^ str ^ "'.")

let eval_expression oc str vars =
  let expr, _ = eval_expression_with_type oc str vars in expr

let rec eval_code oc context =
  let rec _eval_code context =
       let {code; index; vars; scopes; imports} = context in
       let depth = List.length scopes in
       let word, index = get_word code index in
       let context = {code; index; vars; scopes; imports} in
       match find_assoc word control_keywords with
       | Some (scope, func) ->
         let scopes = try
             if word = "sinon" || word = "sinon_si" then
               if List.hd scopes = If then
                 scope :: List.tl scopes
               else
                 syntax_error oc ("Unexpected token '" ^ word ^ "'")
             else
               scope :: scopes
           with Failure _ -> syntax_error oc ("Unexpected token '" ^ word ^ "'") in
         let translation, context = func oc {code; index = (index - String.length word - 1); vars; scopes; imports} in
         let next_translation, context = _eval_code context
         in translation ^ next_translation, context
       | None ->
         match word with
         | "variables" -> _eval_code (eval_variables oc context)
         | "debut" -> eval_code oc context
         | "retourner" -> let expr, index = get_line code index in
           let next, context = _eval_code {code; index; vars; scopes; imports} in
           get_indentation depth ^ "return " ^ eval_expression oc expr vars ^ "\n" ^ next, context
         | "fin" -> (try "", {code; index; vars; scopes = List.tl scopes; imports}
                     with Failure _ -> syntax_error oc "")
         | "afficher" ->
           let expr, index = get_line code index in
           let next, context = _eval_code {code; index; vars; scopes; imports} in
           get_indentation depth ^ "print(" ^ eval_expression oc expr vars ^ ")\n" ^ next, context
         | "" -> if List.length scopes = 0 then "", context else syntax_error oc "Expected 'fin'"
         | _ ->
           let var = get_var_by_name oc word (VarSet.elements vars) in
           let next_word, index = get_word code index in
           if next_word = "<-" then
             let expression, index = get_line code index in
             let expression, type_struct = eval_expression_with_type oc expression vars in
             if type_struct = var.type_struct then
               let next, context = _eval_code {code; index; vars; scopes; imports} in
               get_indentation depth ^ word ^ " = " ^ (eval_expression oc expression vars) ^ "\n" ^ next, context
             else
               type_error oc (Type.string_of_type var.type_struct) (Type.string_of_type type_struct)
           else
             syntax_error oc ("Unexpected token '" ^ next_word ^ "'")
  in _eval_code context

(* This function only adds the new declared variables in the set *)
and eval_variables oc context =
  let {code; index; vars; scopes; imports} = context in
  let rec eval_line vars type_struct = function
      [] -> vars
    | name :: t -> let name = String.trim name in
      eval_line (VarSet.add {name; type_struct} vars) type_struct t
  in let rec _eval_variables vars code index =
       let word, index = get_word code index
       in if word = "debut" then
         vars, index - 6
       else if word = "variables" then
         _eval_variables vars code index
       else
         let line, index = get_line code index in
         let vars = eval_line vars (Type.type_of_string oc word) (String.split_on_char ',' line) in
         _eval_variables vars code index
  in let vars, index = _eval_variables vars code index
  in {code; index; vars; scopes; imports}

and eval_fonction oc context =
  let {code; index; vars; scopes; imports} = context in
  let depth = List.length scopes - 1 in
  (* A function is divided in a header (the name), parameters and a return type.
    This functions combine those parts *)
  let check_return_type i =
    let word, index = get_word code i in
    if word = "->" then
      get_type oc code index
    else
      syntax_error oc ("Expected '->', got '" ^ word ^ "'")
  in
  let name, index = get_word code (index + 9) in (* 9 = 8 + 1 *)
  let names, index, vars = get_param oc vars code index in
  let index, _ = check_return_type index in
  let next, context = eval_code oc {code; index; vars; scopes; imports} in
  let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
  get_indentation depth ^ "def " ^ name ^ "(" ^ names ^ "):\n" ^ next, context

and eval_procedure oc context =
  let {code; index; vars; scopes; imports} = context in
  let depth = List.length scopes - 1 in
  (*Same logic as functions except that there is no need to check a return type*)
  let name, index = get_word code (index + 10) (*10 = 9 + 1*) in
  let names, index, vars = get_param oc vars code index in
  let next, context = eval_code oc {code; index; vars; scopes; imports} in
  let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
  get_indentation depth ^ "def " ^ name ^ "(" ^ names ^ "):\n" ^ next, context

and eval_si oc context =
  let {code; index; vars; scopes; imports} = context in
  let depth = List.length scopes - 1 in
  (* This function checks that the expressions between 'si' and 'alors' is a boolean expression *)
  (* and returns "if <expression>:" followed by the rest of the code *)
  match get_word code index with
  | "si", i -> let expr, index = get_expression oc code i "alors" in
    let expr, type_struct = eval_expression_with_type oc expr vars in
    if type_struct = Type.Boolean then
      let next, context = eval_code oc {code; index; vars; scopes; imports}
      in let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next
      in get_indentation depth ^ "if " ^ expr ^ ":\n" ^ next, context
    else
      type_error oc Type.(string_of_type Boolean) (Type.string_of_type type_struct)
  | _ -> syntax_error oc "si statement must start with 'si'"

and eval_sinon_si oc context =
  let {code; index; vars; scopes; imports} = context in
  let depth = List.length scopes - 1 in
  (* Same as si but sinon_si *)
  match get_word code index with
  | "sinon_si", i -> let expr, index = get_expression oc code i "alors" in
    let expr, type_struct = eval_expression_with_type oc expr vars in
    let next, context = eval_code oc {code; index; vars; scopes; imports}  in
    if type_struct = Type.Boolean then
      if expr = "False" then
        let _, context = eval_code oc {code; index; vars; scopes; imports} in
        "", context
      else
        let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
        get_indentation depth ^ "elif " ^ expr ^ ":\n" ^ next, context
    else
      type_error oc Type.(string_of_type Boolean) (Type.string_of_type type_struct)
  | _ -> syntax_error oc "sinon_si statement must start with 'sinon_si'"

and eval_sinon oc context =
  let {code; index; vars; scopes; imports} = context in
  let depth = List.length scopes - 1 in
  (* Replaces "sinon" by "else:\n"*)
  match get_word code index with
  | "sinon", i -> let next, context = eval_code oc {code; index = i; vars; scopes; imports} in
    let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
    get_indentation depth ^ "else:\n" ^ next, context
  | _ -> syntax_error oc "sinon statement must start with 'sinon'"

and eval_tant_que oc context =
  let {code; index; vars; scopes; imports} = context in
  let depth = List.length scopes - 1 in
  (* Same as si but with 'tant_que' and 'faire' instead of 'si' and 'alors' *)
  match get_word code index with
  | "tant_que", i -> let expr, index = get_expression oc code i "faire" in
    let expr, type_struct = eval_expression_with_type oc expr vars in
    let next, context = eval_code oc {code; index; vars; scopes; imports} in
    let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
    if type_struct = Type.Boolean then
      get_indentation depth ^ "while " ^ expr ^ ":\n" ^ next, context
    else
      type_error oc Type.(string_of_type Boolean) (Type.string_of_type type_struct)
  | _ -> syntax_error oc "tant_que loop must start with 'tant_que'"

and eval_pour_chaque oc context =
  let {code; index; vars; scopes; imports} = context in
  let depth = List.length scopes - 1 in
  (* A pour_chaque instruction has the form "pour_chaque <var> dans <iterable> faire"*)
  (* This function translates it to "for <var> in <iterable>)" *)
  let _, index = get_word code index in
  let var, index = get_expression oc code index "dans"        in let var_expr = eval_expression oc var vars in
  let iterable, index = get_expression oc code index "faire"  in let iterable_expr = eval_expression oc iterable vars in
  let next, context = eval_code oc {code; index; vars; scopes; imports} in
  let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
  get_indentation depth ^ "for " ^ var_expr ^ " in " ^ iterable_expr  ^ ":\n" ^ next, context

and eval_pour oc context =
  let {code; index; vars; scopes; imports} = context in
  let depth = List.length scopes - 1 in
  (* A pour instruction has the form "pour <var> de <start> a <end> faire"*)
  (* This function translates it to "for <var> in range(start, end + 1)" *)
  let _, index = get_word code index in
  let var, index = get_expression oc code index "de"         in let var_expr, var_type = eval_expression_with_type oc var vars in
  let start, index = get_expression oc code index "jusqu_a"  in let start_expr, start_type = eval_expression_with_type oc start vars in
  let end_, index = get_expression oc code index "faire"     in let end_expr, end_type = eval_expression_with_type oc end_ vars in
  if var_type = Type.Int || start_type = Type.Int || end_type = Type.Int then
    let next, context = eval_code oc {code; index; vars; scopes; imports} in
    let next = if next = "" then get_indentation (depth + 1) ^ "pass\n" else next in
    get_indentation depth ^ "for " ^ var_expr ^ " in range(" ^ start_expr ^ ", " ^ end_expr ^ " + 1):\n" ^ next, context
  else
    type_error oc Type.(string_of_type Int) Type.(string_of_type (find_bad_elt None Int [var_type; start_type; end_type]))

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

let translate_code oc code =
  let code = String.trim code and index = 0 and vars = VarSet.empty and scopes = [] and imports = [] in
  let translation, _ = eval_code oc {code; index; vars; scopes; imports} in
  String.trim translation
