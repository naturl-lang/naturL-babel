open Printf

(* USED FOR TEST PURPOSE ONLY. COMMENT FOR COMPILATION *)
(*let exit (_: int) = ()
let stderr = stdout*)

let syntax_error oc message =
  fprintf oc "Syntax error: %s." message;
  exit 2

let type_error oc expected found =
  fprintf oc
    "Type error: An expression was expected of type '%s' but an expression was found of type '%s'."
    expected found;
  exit 2

let name_error oc name =
  fprintf oc "Name error: Name '%s' is not defined." name;
  exit 2

let unknown_type_error oc name =
  fprintf oc "Name error: Type '%s' is not defined." name;
  exit 2

let semantic_error oc message =
  fprintf oc "Semantic error: %s." message;
  exit 2

let unexpected_token_error oc token =
  syntax_error oc ("Unexpected token " ^ token)

let assert_type oc expected found =
  if expected <> found then
    type_error oc expected found
