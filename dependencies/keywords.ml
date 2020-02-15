let eval_code vars code index = code, index + 1

(* Keyword evaluators *)
let eval_fonction vars code index = code, index + 1

let eval_procedure vars code index = code, index + 1

let eval_variables vars code index = code, index + 1

let eval_debut vars code index = code, index + 1

let eval_pour vars code index = code, index + 1

let eval_tant_que vars code index = code, index + 1

let eval_si vars code index = code, index + 1

let eval_sinon vars code index = code, index + 1

let eval_sinon_si vars code index = code, index + 1

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
  ) vars (Buffer.sub code (index) (Buffer.length code)) index

