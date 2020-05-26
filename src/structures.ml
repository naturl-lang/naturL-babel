open Syntax
open Utils
open Errors
open Internationalisation.Translation


module Type = struct

  type t = [
    | `Int
    | `Float
    | `Char
    | `String
    | `Bool
    | `List of t
    | `Function of t list * t
    | `None
    | `Any
    | `Class of t StringMap.t * bool StringMap.t  (* attributes and methodes *)
    | `Custom of string (* name of the class *)
  ]


  let rec to_string : t -> string = function
    | `Int -> "entier"
    | `Float -> "reel"
    | `Char -> "caractere"
    | `String -> "chaine"
    | `Bool -> "booleen"
    | `List `Any -> "liste"
    | `List t -> "liste de " ^ to_string t
    | `Function (params, return) -> (match return with
          `None -> if params = [] then "procedure"
          else"procedure: " ^ String.concat " x " (List.map (fun t -> "(" ^ (to_string t) ^ ")" ) params)
        | _ -> "fonction: " ^ (if params = [] then "_" else String.concat " x " (List.map to_string params)) ^ " -> " ^ to_string return)
    | `None -> "Ø"
    | `Any -> "?"
    | `Class _ -> "type"
    | `Custom name -> name

  let rec of_string (vars: t StringMap.t) str : t =
    let splitted = List.map (fun s -> String.split_on_char '_' s)
        (String.split_on_char ' ' (String.trim str)) in
    match List.flatten splitted with
    | "entier" :: [] -> `Int
    | "reel" :: [] -> `Float
    | "caractere" :: [] -> `Char
    | "chaine" :: [] -> `String
    | "booleen" :: [] -> `Bool
    | "liste" :: [] -> `List `Any
    | "liste" :: "de" :: t -> `List ((of_string vars) (String.concat " " t))
    | ("procedure:" | "procedure") :: [] -> `Function ([], `None)
    | "procedure:" :: tail -> `Function (String.split_on_char 'x' (String.concat " " tail)
                                         |> List.map (fun s -> let s = String.trim s in
                                                       (of_string vars) (String.sub s 1 (String.length s - 2))),
                                         `None)
    | "fonction:" :: tail -> (match Str.split (Str.regexp " -> ") (String.concat " " tail) with
          params :: return :: [] -> `Function ((if String.trim params = "_" then []
                                                else
                                                  (String.split_on_char 'x' params)
                                                  |> List.map (fun s -> let s = String.trim s in
                                                                (of_string vars) (String.sub s 1 (String.length s - 2)))),
                                               of_string vars return)
        | _ -> raise_name_error ((get_string UnknownType) ^ str ^ "'"))
    | ("Ø" | "rien") :: [] -> `None
    | "?" :: [] -> `Any
    | t :: [] -> (match StringMap.find_opt t vars with
        | Some (`Class _) -> `Custom t
        | _ -> raise_name_error ((get_string UnknownType) ^ str ^ "'"))
    | _ -> raise_name_error ((get_string UnknownType) ^ str ^ "'")

  let get_iterable_type : t -> t option = function
      `String -> Some `Char
    | `List t -> Some t
    | _ -> None

  let rec is_compatible (expected: t) (actual: t) =
    expected = `Any || match actual with
    | `Any -> false
    | `None -> true
    | `Int -> expected = `Int || expected = `Float
    | `Char -> expected = `Char || expected = `String
    | `Float | `String | `Bool | `Class _ -> expected = actual
    | `List t1 -> (match expected with  (* A list is covariant *)
        | `List t2 -> is_compatible t2 t1
        | _ -> false)
    | `Function (params1, return1) -> (match expected with  (* A function is covariant in its return type contravariant in its argument type *)
        | `Function (params2, return2) -> List.for_all2 is_compatible params1 params2 && is_compatible return2 return1
        | _ -> false)
    | `Custom _ -> to_string expected = to_string actual

  let get_attr_meths name (vars:t StringMap.t) =
    match (StringMap.find name vars) with
        |`Class (attr_meths, are_set) -> attr_meths, are_set
        | _ -> failwith "Internal missuse of get_attr_meth, No class in context.vars"
end

type scope =
  | If of int
  | While
  | For
  | Function of string * bool
  | Function_definition of string
  | Class_def of string
  | Attributes of string
  | Methods of string

let set_fscope_name scopes name =
  match scopes with
    | Function (_, rflag) :: r -> Function (name, rflag) :: r
    | Function_definition _ :: r -> Function_definition name :: r
    | _ -> failwith "set_fscope_name: Illegal use of the function"

let rec has_returned scopes name =
  match scopes with
    | Function (fname, ret) :: _ when fname = name -> ret
    | [] -> false
    | _ :: r -> has_returned r name

let rec ret scopes name =
  match scopes with
    | Function (fname, _) :: r when fname = name -> Function (name, true) :: r
    | [] -> failwith ("ret: Unexpected error: the function name does not match: " ^ name)
    | e :: r -> e :: ret r name

let rec str_of_scopes scopes =
  match scopes with
    | [] -> "]"
    | If i :: r -> "if " ^ (string_of_int i) ^ ", " ^ str_of_scopes r
    | While :: r -> "while, " ^ str_of_scopes r
    | For :: r -> "for, " ^ str_of_scopes r
    | Function (name, rflag) :: r -> "fun " ^ name ^ " " ^ (string_of_bool rflag) ^ ", " ^ str_of_scopes r
    | Function_definition name :: r -> "fun_def " ^ name ^ ", " ^ str_of_scopes r
    | Class_def name :: r -> "Class_def "^name^", " ^str_of_scopes r
    | Attributes some_shit :: r -> "Attributes declaration, "^some_shit^str_of_scopes r
    | Methods some_shit:: r -> "Methodes declaration, "^some_shit^str_of_scopes r


type line = {
  line: int;
  filename: string;
  scopes: scope list
}

(* The current context of the code *)

type context = {
  filename: string;                      (* The name of the file *)
  code: string;                          (* The whole code *)
  index: int;                            (* The current index in the code *)
  max_index: int option;                 (* If not None, tell when to stop *)
  vars: Type.t StringMap.t;              (* The map of known variables *)
  defs: line StringMap.t;                (* The map of variable definition lines *)
  scopes: scope list;                    (* The stack of scopes *)
}

let empty_context = {
  filename = "<stdin>";
  code = "";
  index = 0;
  max_index = None;
  vars = StringMap.empty;
  defs = StringMap.empty;
  scopes = [];
}

let rec get_current_class_name context =
    match context.scopes with
        | [] -> raise Not_found
        | Class_def name :: _ -> name
        | _ :: r -> get_current_class_name {context with scopes = r}

let print_vars = StringMap.iter (function name -> function t ->
    print_endline ("var " ^ name ^ " : " ^ Type.to_string t))


module Value = struct

  type t =
    | Int of Z.t
    | Float of float
    | Char of char
    | String of string
    | Bool of bool
    | Variable of string
    | Instance of string * string
    | None

  let to_string = function
      Int i -> Z.to_string i
    | Float f -> string_of_float f
    | Char c -> "'" ^ String.make 1 c ^ "'"
    | String s -> {|"|} ^ s ^ {|"|}
    | Bool b -> if b then "True" else "False"
    | Variable name -> resolve_py_conficts name
    | Instance (type_def, name) -> resolve_py_conficts type_def ^ "." ^ (resolve_py_conficts name)
    | None -> "None"

  let of_string = function str ->
    let str = String.trim str
    and name_re = "[_a-zA-Z\\.][_a-zA-Z0-9\\.]*" in
    if Str.string_match (Str.regexp "^-?[0-9]+$") str 0 then
      Int (Z.of_string str)
    else if Str.string_match (Str.regexp {|^[0-9]+.[0-9]*$|}) str 0 then
      Float (float_of_string str)
    else if Str.string_match (Str.regexp {|^'\(.\)'$|}) str 0 then
      Char (Str.matched_group 1 str).[0]
    else if Str.string_match (Str.regexp {|^"\(.*\)"$|}) str 0 then
      String (Str.matched_group 1 str)
    else if str = "vrai" || str = "faux" then
      Bool (str = "vrai")
    else if str = "nul" then
      None
    else if Str.string_match (Str.regexp ("^" ^ name_re ^ "$")) str 0 then
      Variable str
    else if Str.string_match (Str.regexp ("^instance " ^ name_re ^ "$")) str 0 then
      Instance ("self", String.sub str 9 (String.length str - 9))
    else if str = "" then
      raise_syntax_error (get_string ExpectedOperand)
    else
      raise_syntax_error (get_string InvalidExpression)


  open (struct
    let accessed_var_regex = Str.regexp "\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\(\\(\\.[a-zA-Z_][a-zA-Z_0-9]*\\)+\\)"
  end)

  let rec get_unknown_variable context name error =
    if Str.string_match accessed_var_regex name 0 then
      let attribute = Str.matched_group 2 name in
      let attribute = String.sub attribute 1 (String.length attribute - 1) in
      let name = Str.matched_group 1 name in
      let class_name = if name = "self" then get_current_class_name context
        else match StringMap.find name context.vars with
            `Custom name -> name
          | t -> raise_name_error ("Type " ^ (Type.to_string t) ^ " has no attribute " ^ attribute)
      in match StringMap.find_opt class_name context.vars with
      | Some `Class (attr_meths, _) -> (match StringMap.find_opt attribute attr_meths with
          | Some t -> t
          | _ -> if Str.string_match accessed_var_regex attribute 0 then
              get_unknown_variable {context with vars = StringMap.union (fun _ -> fun _ -> fun value -> Some value) context.vars attr_meths} attribute error
            else
              raise_type_error ("Type " ^ class_name ^ " has no attribute " ^ attribute))
      | None -> raise_type_error ("Undefined type " ^ class_name)
      | _ -> assert false
    else
      error ()

  let rec get_type context = function
    | Int _ -> `Int
    | Float _ -> `Float
    | Char _ -> `Char
    | String _ -> `String
    | Variable name -> (try StringMap.find name context.vars
                        with Not_found -> get_unknown_variable context name
                                            (fun () ->  raise_name_error ((get_string UnknownVariable) ^ name ^ "'")))
    | Bool _ -> `Bool
    | Instance (type_name, name) ->
      let type_name = if type_name = "self" then get_current_class_name context else type_name in
      let attr_meths, are_set = try Type.get_attr_meths type_name context.vars
        with Not_found -> raise_name_error ((get_string UnknownVariable) ^ type_name ^ "'") in
      let attr_type, are_set = try StringMap.find name attr_meths, StringMap.find name are_set
        with Not_found -> if Str.string_match accessed_var_regex name 0 then
            let attr_name = Str.matched_group 1 name in
            let sub_attr_name = Str.matched_group 2 name in
            let sub_attr_name = String.sub sub_attr_name 1 (String.length sub_attr_name - 1) in
            match StringMap.find_opt attr_name attr_meths with
            | Some t -> let vars = context.vars |> StringMap.add attr_name t in
              get_type { context with vars } (Instance (Type.to_string t, sub_attr_name)),
              (try StringMap.find name are_set with Not_found -> true)
            | None -> raise_name_error ((get_string UnknownVariable) ^ attr_name ^ "' in class '" ^ type_name ^ "'")
          else
            raise_name_error ((get_string UnknownVariable) ^ name ^ "' in class '" ^ type_name ^ "'") in
      if are_set then
        attr_type
      else
        raise_name_error ("The variable " ^ name ^ " has no value in the current context")
    | None -> `None
end


module Expr = struct

  type t =
    | Plus of t * t           (* x + y *)
    | Minus of t * t          (* x * y *)
    | Neg of t                (* -x *)
    | Times of t * t          (* x * y *)
    | Div of t * t            (* x / y *)
    | Div_int of t * t        (* x div y *)
    | Modulus of t * t        (* x mod y *)
    | Pow of t * t            (* x ^ y *)
    | Eq of t * t             (* x = y *)
    | Gt of t * t             (* x > y *)
    | Gt_eq of t * t          (* x >= y *)
    | Lt of t * t             (* x < y *)
    | Lt_eq of t * t          (* x <= y *)
    | And of t * t            (* x et y *)
    | Or of t * t             (* x ou y *)
    | Not of t                (* non x *)
    | List of t list          (* [x, y, ...] *)
    | Call of string * t list (* <function-name>(x, y, ...) *)
    | Subscript of t * t      (* list[x] *)
    | Value of Value.t        (* x *)
end
