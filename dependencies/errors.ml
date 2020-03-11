open Printf

(* USED FOR TEST PURPOSE ONLY. COMMENT FOR COMPILATION *)
(*let exit (_: int) = ()
let stderr = stdout*)

(* Leave this one uncommented *)
let prerrf arg = fprintf stderr arg

let syntax_error message =
  prerrf "Syntax error: %s." message;
  exit 2

let type_error expected found =
  prerrf
    "Type error: An expression was expected of type '%s' but an expression was found of type '%s'."
    expected found;
  exit 2

let name_error name =
  prerrf "Name error: Name '%s' is not defined." name;
  exit 2

let unknown_type_error name =
  prerrf "Name error: Type '%s' is not defined." name;
  exit 2

let semantic_error message =
  prerrf "Semantic error: %s." message;
  exit 2

let assert_type expected found =
  if expected <> found then
    type_error expected found
