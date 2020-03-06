#mod_use "structures.ml";;
#mod_use "getters.ml";;

open Str
open Structures
open Getters

(* Used mostly for debugging *)
let print_var_set set =
  let rec print_var_list = function
      [] -> print_endline "}"
    | var :: t ->
      print_char '\t';
      print_variable (var);
      print_var_list t
  in print_endline "{"; print_var_list (VarSet.elements set) ;;

let eval_expression str =
  let separators = "\\([\t() ]?\\)"
  and replacements = [
    "et", "and";
    "ou", "or";
    "non", "not";
    "=", "==";
    "vrai", "True";
    "faux", "False"
  ] in
  let rec _replace str = function
    | [] -> str
    | (prev, new_word) :: t ->
      let r = regexp (separators ^ prev ^ separators)
      in _replace (global_replace r ("\\1" ^ new_word ^ "\\2") str) t
  in _replace str replacements

let rec eval_code vars code index = 


(* This function only adds the new declared variables in the set *)
and eval_variables vars code index =
  let rec eval_line vars type_struct = function
     [] -> vars
    | name :: t -> let name = String.trim name in
      eval_line (VarSet.add {name; type_struct} vars) type_struct t
  in let word, index = get_word code index
  in if word = "fin" then
    vars, index
  else if word = "variables" then
    eval_variables vars code index
  else
    let line, index = get_line code index in
    let vars = eval_line vars (Type.type_of_string word) (String.split_on_char ',' line) in
    eval_variables vars code index

(* Keyword evaluators *)
and eval_fonction vars code index depth =
  (*The function is divided in a header (the name), and a rest, this fuctions gets the
    header and combines it with the rest*)
  let rec check_return_type i =
    match Buffer.nth code i with
      ' '| ':' -> check_return_type (i + 1)
    | _ ->  get_type code i
  in
  let name,index = get_word code (index + 9) in (*9 = 8 + 1*)
  let _, variables, index = get_param vars "" code (ignore_chrs code index) in
  let index, _ = check_return_type index in
  (depth ^ "def " ^ name ^ "(" ^ variables ^ "):\n", index+1)

and eval_procedure vars code index depth =
  (*Same logic as the function except that there is no need to check a return type*)
  let name, index = get_word code (index + 10) (*10 = 9 + 1*) in
  let _, variables, index = get_param vars "" code (ignore_chrs code index) in
  (depth ^ "def " ^ name ^ "(" ^ variables ^ "):\n", index + 1)


and eval_si code index depth =
  match get_word code index with
  |("si", i) -> let a,b = get_expression code i "alors" in
    (depth ^ "if " ^ a ^ ":\n", b)
  |(_, _) -> failwith("Syntax Error : condition must start with si")


and eval_tant_que code index depth =
  match get_word code index with
  |("tant_que", i) -> let a, b = get_expression code i "faire" in
    (depth ^ "while " ^ a ^":\n", b)
  |(_, _) -> failwith("Syntax Error : tantque loop must start with tantque")

and eval_sinon code index depth =
  match get_word code index with
  |("sinon", i) -> (depth ^ "else:\n", i)
  |(_, _) -> failwith("Syntax Error: sinon condition must start with sinon")


(*and eval_debut vars code index = code, index+1*)


let eval_sinon_si code index depth =
  match get_word code index with
  |("sinon_si", i) -> let a,b = get_expression code i "alors" in
    (depth ^ "elif " ^ a ^ ":\n", b)
  |(_, _) -> failwith("Syntax Error : sinon_si condition must start with sinon_si");;

let eval_pour_chaque code index depth =
  let rec get_foreach_core code index =
    match get_word code index with
    |("de", i) -> let a, b = get_foreach_core code (i) in (" in" ^ a, b)
    |("faire", i) -> ("", i+1)
    |(word, i) -> let a, b = get_foreach_core code (i) in (" " ^ word ^ a, b)
  in
  match get_word code index with
  |("pour_chaque", i) -> let a,b = get_foreach_core code i in
    (depth ^ "for" ^ a ^":\n", b)
  |(_, _) -> failwith("Syntax Error : pour_chaque loop must start with pour_chaque")


let eval_pour code index depth =
  let rec get_for_core code index =
    match get_word code index with
    | ("faire", i) -> ("", i)
    | ("de", i) -> let a,b = (get_for_core code (i)) in (" in range("^ a, b)
    | ("jusqu_a", i) -> let a, b = get_for_core code (i) in
      ("," ^ a ^ " + 1)", b+1)
    | (word, i) -> let a, b = get_for_core code (i) in (" " ^ word ^ a, b)
  in
  match get_word code index with
  |("pour", i) -> let a, b = get_for_core code i in 
    (depth ^ "for" ^ a ^ ":\n", b)
  |(_, _) -> failwith("Syntax Error : pour loop must start with pour")

