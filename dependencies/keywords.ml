open Structures
(*#mode_use "structures.ml";;*) (*UNCOMMENT FOR TOPLEVEL*)
(*AUX
  -Gets the next word if not EOF: 
  get_word: Buffer.T -> string (empty string) -> int -> string = <fun>*)
let rec get_word code word i  =
  match Char.escaped (Buffer.nth code i) with
  " " | "" |"\n"|"("|","|")"-> (word,i)
  |_ when i = Buffer.length code -> (word,i)
  |x -> get_word code (word^x) (i+1);; 

(*Check if the specified type is a valid builtin type, if that is the case, returns the right type *)
let get_type code index =
  let _get_type = function
    |"entier" -> Int
    |"reel" -> Float
    |"octet" -> failwith "[Type_Error]: Type byte not implemented"
    |"booleen" -> Boolean
    |"caractere" -> Char
    |"chaine" -> String
    |x -> (print_string x ; failwith "get_type: Undefined type")
  in
  let (t,index) = get_word code "" index in
 (*  print_string t; print_string "\n";*) (*Debug*)
  (index,_get_type t) ;;

(*Gets the variables of the function or the procedure*)

let rec get_vars vars variables code i =
  if i = Buffer.length code then
    failwith "get_vars: Unclosed ("
  else
    match Buffer.nth code i with
    ')' -> (vars, String.sub variables 0 (String.length variables - 1), i+1) (*Returns afeter )*) (*deletes the last ","*)
    |' '|',' -> get_vars vars variables code (i+1) (*skips whitespaces and comas*)
    |_ -> (if variables = "" then (print_string "Empty";)
        else print_string variables;
        let (i, type_) = get_type code i in
        let (name, i) = get_word code "" (i+1) in (*+1 to skip the white space*)
        get_vars ((name, type_)::vars) (variables^name^",") code i
);;

(*Core*)

let eval_code vars code index = code, index + 1

(* Keyword evaluators *)
let eval_fonction vars code index =
(*The function is divided in a header (the name), and a rest, this fuctions gets the
header and combines it with the rest*)
  let rec check_return_type i =
    match Buffer.nth code i with
    ' '| ':' -> check_return_type (i+1)
    |_-> get_type code i
  in
  let name,index = get_word code "" (index+9) in (*9 = 8 + 1*)
  let vars, variables, index = get_vars vars "" code (index+1) in
  let index, ret_type = check_return_type index in
  let rest, index = eval_variables vars code index in
 ("def "^name^"("^variables^"):\n"^rest, index+1)

let eval_procedure vars code index =
(*Same logic as the function except that there is no need to check a return type*)
  let name, index = get_word code "" (index+10) (*10 = 9 +1*) in
  let vars, variables, index = get_vars vars "" code (index+1) in
  let rest, index = eval_variables vars code index in
  ("def "^name^"("^variables^")\n"^rest, index+1)

let eval_variables vars code index = "code", index

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

(*TESTS DELETE BEFORE ORAL_PUSH*)
(*POC of the implemented algos :
         - eval_fonction
         - eval_procedure *)
let buf = Buffer.create 10;;
Buffer.add_string  buf "fonction shla(entier a, reel b) : booleen ";;
let (s,i) = eval_fonction [] buf 0;;
i==42 ;; (*42 = len(buf)-1 + 1 which gets to the next instructions*)
print_string (s^"\n");;

let buf = Buffer.create 10;;
Buffer.add_string buf "procedure doodi(entier a, chaine b) "
let(s,i) = eval_procedure [] buf 0;;
i==36;;
print_string (s^"\n");;
