(*#load "str.cma";;*)
open Errors

type token =
  | Litteral of string
  | Identifier of string
  | Operator of string
  | OpenP
  | CloseP
  | Coma
  | OpenHook
  | CloseHook;;

let print_token token =
  let token_to_string = function
    | Litteral a -> "Litteral " ^ a
    | Identifier a -> "Identifier " ^ a
    | Operator a -> "Operator " ^ a
    | OpenP -> "OpenP"
    | CloseP -> "CloseP"
    | Coma -> "Coma"
    | OpenHook -> "OpenHook"
    | CloseHook -> "CloseHook"
  in print_endline (token_to_string token);;

let print_tokens tokens = List.iter print_token tokens;;

let tokenize input =
  let _tokenize input =
    let reg_identifier = Str.regexp "[a-zA-Z_][a-zA-Z_0-9]*"
    and reg_boolean = Str.regexp "vrai\\|faux"
    and reg_number = Str.regexp "[0-9]+\\.?[0-9]*"
    and reg_operator = Str.regexp {|ou\|et\|non\|=\|!=\|<=\|>=\|<\|>\|*\|fois\|+\|-\|/\|div\|mod|}
    and reg_string = Str.regexp  {|"\([^"]\|\"\)*"|}
    and reg_char = Str.regexp "'[a-Za-z0-9]*'"
    and reg_openp = Str.regexp "("
    and reg_closep = Str.regexp ")"
    and reg_coma = Str.regexp ","
    and reg_openhook = Str.regexp "\\["
    and reg_closehook = Str.regexp "\\]"
    and length = String.length input  in
    let rec _tokenize input index =
      if index = length then
        []
      else
      if input.[index] = ' ' then
        _tokenize input (index+1)
      else
      if Str.string_match reg_operator input index then
        let token = Str.matched_string input in
        (Operator token) :: _tokenize input (index + (String.length token))
      else if Str.string_match reg_number input index || Str.string_match reg_string input index
              || Str.string_match reg_boolean input index || Str.string_match reg_char input index then
        let token = Str.matched_string input in
        (Litteral token) :: _tokenize input (index + (String.length token))
      else if Str.string_match reg_identifier input index then
        let token = Str.matched_string input in
        if List.mem token Syntax.keywords then
          raise_syntax_error ("Invalid token in expression: '" ^ token ^ "' is a reserved keyword")
        else
          (Identifier token) :: _tokenize input (index + (String.length token))
      else if Str.string_match reg_openp input index then
        OpenP :: _tokenize input (index+1)
      else if Str.string_match reg_closep input index then
        CloseP :: _tokenize input (index+1)
      else if Str.string_match reg_coma input index then
        Coma :: _tokenize input (index+1)
      else if Str.string_match reg_openhook input index then
        OpenHook :: _tokenize input (index+1)
      else if Str.string_match reg_closehook input index then
        CloseHook :: _tokenize input (index+1)
      else
        raise_syntax_error "Could not capture the unknown token"
    in _tokenize input 0
  in
  let improve_tokens tokens =
    let rec _improve_tokens ?previous ?(par_count = 0) ?(hook_count = 0) = function
      | [] -> if par_count > 0 then raise_syntax_error "Missing closing parenthesis ')'"
        else if hook_count > 0 then raise_syntax_error "Missing closing bracket ']'"
        else []
      | OpenP :: t -> OpenP :: _improve_tokens t ~previous: OpenP ~par_count: (par_count + 1) ~hook_count
      | CloseP :: t -> if par_count = 0 then raise_syntax_error "Unexpected token: ')'"
          else CloseP :: _improve_tokens t ~previous: CloseP ~par_count: (par_count - 1) ~hook_count
      | OpenHook :: t -> (match previous with
          | None | Some (Operator _ | OpenP | OpenHook | Coma) -> Operator "[" :: OpenHook :: _improve_tokens t ~previous: OpenHook ~par_count ~hook_count: (hook_count + 1)
          | _ -> Operator "get[" :: OpenHook :: _improve_tokens t ~previous: OpenHook ~par_count ~hook_count: (hook_count + 1))
      | CloseHook :: t -> if hook_count = 0 then raise_syntax_error "Unexpected token: ']'"
          else CloseHook :: _improve_tokens t ~previous: CloseHook ~par_count ~hook_count: (hook_count - 1)
      | Identifier name :: OpenP :: t -> Operator name :: _improve_tokens (OpenP :: t) ~previous: (Operator name) ~par_count ~hook_count
      | Operator "-" :: t -> let op = match previous with
          | None | Some (Operator _ | OpenP | OpenHook | Coma) -> Operator "neg"
          | _ -> Operator "-"
        in op :: _improve_tokens t ~previous: op ~par_count ~hook_count
      | h :: t -> h :: _improve_tokens t ~previous: h ~par_count ~hook_count
    in _improve_tokens tokens
  in improve_tokens (_tokenize input)
