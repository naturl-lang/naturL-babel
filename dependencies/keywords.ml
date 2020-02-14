(* Keyword evaluators *)
let eval_fonction str index = str, index + 1

let eval_procedure str index = str, index + 1

let eval_variables str index = str, index + 1

let eval_debut str index = str, index + 1

let eval_pour str index = str, index + 1

let eval_tant_que str index = str, index + 1

let eval_si str index = str, index + 1

let eval_sinon str index = str, index + 1

let eval_sinon_si str index = str, index + 1

let keywords_list = [
    "fonction";
    "procedure";
    "debut";
    "pour";
    "tant_que";
    "si";
    "sinon";
    "sinon_si"
  ]

(* Translates the code after the keyword and return an index to continue evaluating the code *)
(*NB: The terminating keywords such as "fin" or "faire" are ignored here.*)
(*    We only consider the keywords starting a new scope*)
let evaluate_keyword keyword =
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
  ) keyword
