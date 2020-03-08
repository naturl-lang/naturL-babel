open Str
open Structures
open Getters
open Expressions


let print_var_set set =
  let rec print_var_list = function
      [] -> print_endline "}"
    | var :: t ->
      print_char '\t';
      print_variable (var);
      print_var_list t
  in print_endline "{"; print_var_list (VarSet.elements set)

let create_buffer str =
  let buf = Buffer.create (String.length str) in
  Buffer.add_string buf str;
  buf

let search_variable_by_name name vars =
  let rec _search_variable_by_name n = function
    | [] -> false
    | {name; _} :: t -> name = n || _search_variable_by_name n t
  in _search_variable_by_name name (VarSet.elements vars)

let eval_expression_with_type str vars =
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
  let is_valid, type_struct = check_expr str vars
  in if is_valid then
    String.trim (_replace str replacements), type_struct
  else
    failwith "Type error: Inconsistent expression"

let eval_expression str vars =
  let expr, _ = eval_expression_with_type str vars in expr


let rec eval_code ?(is_si = false) vars code index depth =
  let rec find_assoc key = function
    | (k, value) :: t -> if key = k then Some value else find_assoc key t
    | [] -> None
  in let control_keywords =
       [
         "fonction", eval_fonction;
         "procedure", eval_procedure;
         "si", eval_si;
         "sinon", eval_sinon;
         "sinon_si", eval_sinon_si;
         "pour", eval_pour;
         "pour_chaque", eval_pour_chaque;
         "tant_que", eval_tant_que
       ]
  in let rec _eval_code vars code index depth =
       let word, index = get_word code index
       in match find_assoc word control_keywords with
       | Some func ->
         let current_depth =
           if is_si && word = "sinon_si" || word = "sinon" then
             String.sub depth 0 (String.length depth - 4)
           else depth in
         let translation, index, vars = func vars code (index - String.length word - 1) current_depth in
         let next_translation, index, vars = _eval_code vars code index depth
         in translation ^ next_translation, index, vars
       | None ->
         match word with
         | "variables" -> let vars, index = eval_variables vars code index
           in _eval_code vars code index depth
         | "debut" -> _eval_code vars code index depth
         | "retourner" -> let expr, index = get_line code index in
           let next, index, vars = _eval_code vars code index depth in
           depth ^ "return " ^ eval_expression expr vars ^ "\n" ^ next, index, vars
         | "fin" -> "", index, vars
         | "afficher" ->
           let expr, index = get_line code index in
           let next, index, vars = _eval_code vars code index depth in
           depth ^ "print(" ^ eval_expression expr vars ^ ")\n" ^ next, index, vars
         | "" -> "", index, vars
         | _ ->
           if search_variable_by_name word vars then
             let next_word, index = get_word code index in
             if next_word = "<-" then
               let expression, index = get_line code index in
               let next, index, vars = _eval_code vars code index depth in
               depth ^ word ^ " = " ^ (eval_expression expression vars) ^ "\n" ^ next,
               index, vars
             else
               failwith ("Syntax error: Unexpected token: '" ^ next_word ^ "'")
           else
             failwith ("Name error: Name '" ^ word ^ "' is not defined")
  in _eval_code vars code index depth

(* This function only adds the new declared variables in the set *)
and eval_variables vars code index =
  let rec eval_line vars type_struct = function
     [] -> vars
    | name :: t -> let name = String.trim name in
      eval_line (VarSet.add {name; type_struct} vars) type_struct t
  in let word, index = get_word code index
  in if word = "debut" then
    vars, index - 6
  else if word = "variables" then
    eval_variables vars code index
  else
    let line, index = get_line code index in
    let vars = eval_line vars (Type.type_of_string word) (String.split_on_char ',' line) in
    eval_variables vars code index

and eval_fonction vars code index depth =
  (* The function is divided in a header (the name), and a rest, this fuctions gets the
    header and combines it with the rest *)
  let check_return_type i =
    let word, index = get_word code i in
    if word = "->" then
      get_type code index
    else
      failwith ("Syntax error: Expected '->', got '" ^ word ^ "'")
  in
  let name, index = get_word code (index + 9) in (* 9 = 8 + 1 *)
  let names, index, vars = get_param vars code index in
  let index, _ = check_return_type index in
  let next, index, vars = eval_code vars code index (depth ^ "    ") in
  depth ^ "def " ^ name ^ "(" ^ names ^ "):\n" ^ next, index, vars

and eval_procedure vars code index depth =
  (*Same logic as the function except that there is no need to check a return type*)
  let name, index = get_word code (index + 10) (*10 = 9 + 1*) in
  let names, index, vars = get_param vars code index in
    let next, index, vars =  eval_code vars code index (depth ^ "    ") in
  depth ^ "def " ^ name ^ "(" ^ names ^ "):\n" ^ next, index + 1, vars

and eval_si vars code index depth =
  match get_word code index with
  | "si", i -> let expr, index = get_expression code i "alors" in
    let expr, type_struct = eval_expression_with_type expr vars in
    if type_struct = Type.Boolean then
      let next, index, vars = eval_code vars code index (depth ^ "    ") ~is_si:true
      in depth ^ "if " ^ expr ^ ":\n" ^ next, index, vars
    else
      failwith ("Type error: This expression has type " ^ Type.string_of_type type_struct ^ " but an expression was expected of type boolean")
  | _ -> failwith "Syntax Error: condition must start with si"

and eval_tant_que vars code index depth =
  match get_word code index with
  | "tant_que", i -> let expr, index = get_expression code i "faire" in
    let expr, type_struct = eval_expression_with_type expr vars in
    let next, index, vars =  eval_code vars code index (depth ^ "    ") in
    if type_struct = Type.Boolean then
      depth ^ "while " ^ expr ^ ":\n" ^ next, index, vars
    else
      failwith ("Type error: This expression has type " ^ Type.string_of_type type_struct ^ " but an expression was expected of type boolean")
  | _ -> failwith "Syntax Error: tant_que loop must start with tant_que"

and eval_sinon vars code index depth =
  match get_word code index with
  | "sinon", i -> depth ^ "else:\n", i, vars
  | _ -> failwith "Syntax Error: sinon condition must start with sinon"

and eval_sinon_si vars code index depth =
  match get_word code index with
  | "sinon_si", i -> let expr, index = get_expression code i "alors" in
    let expr, type_struct = eval_expression_with_type expr vars in
    let next, index, vars =  eval_code vars code index (depth ^ "    ") ~is_si: true in
    if type_struct = Type.Boolean then
      depth ^ "elif " ^ expr ^ ":\n" ^ next, index, vars
    else
      failwith ("Type error: This expression has type " ^ Type.string_of_type type_struct ^ " but an expression was expected of type boolean")
  | _ -> failwith "Syntax Error: sinon_si condition must start with sinon_si"

and eval_pour_chaque vars code index depth =
  let rec get_foreach_core code index =
    match get_word code index with
    | "dans", i -> let expr, index = get_foreach_core code i in
      " in" ^ expr, index
    | "faire", i  -> "", i + 1
    | word, i  -> let expr, index = get_foreach_core code i in
      " " ^ word ^ expr, index
  in
  match get_word code index with
  | "pour_chaque", i -> let expr, index = get_foreach_core code i in
    let next, index, vars = eval_code vars code index (depth ^ "    ") in
    depth ^ "for" ^ expr ^ ":\n" ^ next, index, vars
  | _ -> failwith "Syntax Error: pour_chaque loop must start with pour_chaque"

and eval_pour vars code index depth =
  let rec get_for_core code index =
    match get_word code index with
    | "faire", i -> "", i
    | "de", i -> let expr, index = (get_for_core code i) in
      " in range(" ^ expr, index
    | "jusqu_a", i -> let expr, index = get_for_core code i in
      ", " ^ expr ^ " + 1)", index + 1
    | word, i -> let expr, index = get_for_core code i in
      word ^ expr, index
  in
  match get_word code index with
  | "pour", i -> let expr, index = get_for_core code i in
    let next, index, vars = eval_code vars code index (depth ^ "    ") in
    depth ^ "for " ^ expr ^ ":\n" ^ next, index, vars
  | _ -> failwith "Syntax Error: pour loop must start with pour"

let translate_code code =
  let buf = create_buffer (String.trim code) in
  let translation, _, _ = eval_code VarSet.empty buf 0 "" in
  translation
