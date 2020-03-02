(*#mod_use "structures.ml";;
#mod_use "getters.ml";;
*)
open Structures;;
open Getters;;

module VarSet = Set.Make(struct type t = variable let compare = compare end)

(* Used mostly for debugging *)
let print_var_set set =
  let rec print_var_list = function
      [] -> print_endline "}"
    | var :: t ->
      print_char '\t';
      print_variable (var);
      print_var_list t
  in print_endline "{"; print_var_list (VarSet.elements set) ;;

let eval_code _ code index = code, index + 1

(* This function only adds the new declared variables in the set *)
let rec eval_variables vars code index =
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
let eval_fonction vars code index depth =
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

let eval_procedure vars code index depth =
  (*Same logic as the function except that there is no need to check a return type*)
  let name, index = get_word code (index + 10) (*10 = 9 + 1*) in
  let _, variables, index = get_param vars "" code (ignore_chrs code index) in
  (depth ^ "def " ^ name ^ "(" ^ variables ^ "):\n", index + 1);;



let get_expression code index =
  let rec _get_expression code i =
    match get_word_and_returns code i with
    |("alors", k) -> (Buffer.sub code index (k-8), k)
    |("\n", _) -> failwith("Syntax Error : no alors was found on the line")
    |("\r", _) -> failwith("Syntax Error : no alors was found on the line")
    |(_, _) -> _get_expression code (i+1)
  in _get_expression code index;;


let eval_si code index depth =
  match get_word code index with
  |("si", i) -> let a,b = get_expression code (index+i) in
    (depth ^ "if " ^ a ^ ":\n", b)
  |(_, _) -> failwith("Syntax Error : condition must start with si");;


(*let buffer = Buffer.create 1;;
Buffer.add_string buffer "si a > b && c > (d+8) \n";;

get_word buffer 0;;
eval_si buffer 0 "";;
*)
(*
let eval_debut vars code index depth = "code", index ;;

let eval_pour vars code index = code, index + 1 ;;

let eval_tant_que vars code index = code, index + 1 ;;

 

let eval_sinon vars code index = code, index + 1 ;;

let eval_sinon_si vars code index = code, index + 1 ;;
*)

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
(*
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
*)
