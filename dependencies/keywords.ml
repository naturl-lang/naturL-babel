(*#mod_use "structures.ml";;
  #mod_use "type_operations.ml";;
  #mod_use "getters.ml"
  #load "str.cma";; *) (*UNCOMMENT FOR TOPLEVEL*)

open Structures;;
open Str;;
open Type_operations;;
open Getters;;


let eval_code vars code index = code, index + 1

(* Keyword evaluators *)
let eval_fonction vars code index depth =
  (*The function is divided in a header (the name), and a rest, this fuctions gets the
    header and combines it with the rest*)
  let rec check_return_type i =
    match Buffer.nth code i with
      ' '| ':' -> check_return_type (i + 1)
    | _ ->  get_type code i
  in
  let name,index = get_word code "" (index + 9) in (*9 = 8 + 1*)
  let vars, variables, index = get_param vars "" code (ignore_chrs code index) in
  let index, ret_type = check_return_type index in
  let rest, index = eval_variables vars code index (depth ^ "\t") in
  (depth ^ "def " ^ name ^ "(" ^ variables ^ "):\n" ^ rest, index+1);;

let eval_procedure vars code index depth =
  (*Same logic as the function except that there is no need to check a return type*)
  let name, index = get_word code "" (index + 10) (*10 = 9 + 1*) in
  let vars, variables, index = get_param vars "" code (ignore_chrs code index) in
  let rest, index = eval_variables vars code index (depth ^ "\t")in
  (depth^"def " ^ name ^ "(" ^ variables ^ "):\n" ^ rest, index + 1);;

let eval_variables vars code index depth =
  (**)
  let index = (ignore_chrs code index) + 9 in
  let len = Buffer.length code in
  let rec get_values i pword vars =
    (*Gets in the code in order to find the first values of the variables of var if set*)
    if i >= len then
      ()
    else
      let word, i = get_word code "" (ignore_chrs code i) in
      if word = "<-" then
        let var = get_var_by_name pword vars in
        match var._type with
          _ when var.value <> "None!" -> get_values (ignore_chrs code i) "" vars
        | Type.Char -> (let value, i = get_char code (ignore_chrs code i) in (var.value <- ("'"^value^"'") ;
                                                                             get_values i value vars))
        | Type.String -> (let value, i = get_str code (ignore_chrs code i) in (var.value <- ("\""^value^"\"") ;
                                                                              get_values i value vars))
        | _ -> (let value, i = get_word code "" (ignore_chrs code i) in (var.value <- value ;
                                                                       get_values i value vars))
      else
        get_values i word vars
  in
  let rec get_vars vars i current_type = (*adds to var the variables*)
    (*type1 : var1, var2, ..., varN*)
    if i >= len then
      failwith "eval_variables: get_vars: No begening of the fonction/procedure found"
    else
      let word, i = get_word code "" i in
      match word with
      | "debut" -> index,vars
      | new_type when is_type word -> get_vars vars (ignore_chrs code i) (string_to_type new_type)
      | name -> get_vars ({name = name; _type = current_type; value = "None!"; is_parametre = false} :: vars)
                  (ignore_chrs code i) current_type
  in
  let rec get_result vars result =
    match vars with
      [] -> result
    | var :: r when not (var.is_parametre) -> get_result r (result^depth^(var.name)^" = "^(var.value)^"\n")
    | _ :: r -> get_result r result
  in
  let index, current_type = get_type code (ignore_chrs code ((ignore_chrs code index) + 1)) in (*variables:*)
  let index, vars = get_vars vars (ignore_chrs code index) current_type in
  get_values (ignore_chrs code index) "" vars;
  let rest, index = eval_debut vars code index depth in
  ((get_result vars "") ^ rest, index);;


let eval_debut vars code index depth = "code", index ;;

let eval_pour vars code index = code, index + 1 ;;

let eval_tant_que vars code index = code, index + 1 ;;

let eval_si vars code index = code, index + 1 ;;

let eval_sinon vars code index = code, index + 1 ;;

let eval_sinon_si vars code index = code, index + 1 ;;

let keywords_list = [
  "fonction";
  "procedure";
  "debut";
  "pour";
  "tant_que";
  "si";
  "sinon";
  "sinon_si"
];;

(* Translates the code after the keyword and returns an index to continue evaluating the code *)
(*NB: The terminating keywords such as "fin" or "faire" are ignored here.*)
(*    We only consider the keywords starting a new scope*)
let evaluate_keyword keyword vars code index =
  (function
    | "fonction" -> eval_fonction
    | "procedure" -> eval_procedure
    | "variables" -> eval_variables
    | "debut" -> eval_debut
    | "pour" -> eval_pour
    | "tant_que" -> eval_tant_que
    | "si" -> eval_si
    | "sinon" -> eval_sinon
    | "sinon_si" -> eval_sinon_si
    | token -> invalid_arg ("evaluate_keyword: Unknown token: " ^ token)
  ) vars (Buffer.sub code (index) (Buffer.length code)) index ;;
