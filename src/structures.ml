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
    | `Custom of string * t StringMap.t * bool StringMap.t  (*name, attributes and methodes*)
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
        | _ -> "fonction: " ^ String.concat " x " (List.map to_string params) ^ " -> " ^ to_string return)
    | `None -> "Ø"
    | `Any -> "?"
    | `Custom (name,_,_) -> name 


  let rec of_string str : t =
    let splitted = List.map (fun s -> String.split_on_char '_' s)
        (String.split_on_char ' ' (String.trim str)) in
    match List.flatten splitted with
    | "entier" :: [] -> `Int
    | "reel" :: [] -> `Float
    | "caractere" :: [] -> `Char
    | "chaine" :: [] -> `String
    | "booleen" :: [] -> `Bool
    | "liste" :: [] -> `List `Any
    | "liste" :: "de" :: t -> `List (of_string (String.concat " " t))
    | "procedure" :: [] -> `Function ([], `None)
    | "procedure:" :: tail -> `Function (String.split_on_char 'x' (String.concat " " tail)
                                         |> List.map (fun s -> let s = String.trim s in
                                                       of_string (String.sub s 1 (String.length s - 2))),
                                         `None)
    | "fonction:" :: tail -> (match Str.split (Str.regexp " -> ") (String.concat " " tail) with
          params :: return :: [] -> `Function ((String.split_on_char 'x' params)
                                               |> List.map (fun s -> let s = String.trim s in
                                                             of_string (String.sub s 1 (String.length s - 2))),
                                               of_string return)
        | _ -> raise_name_error ((get_string UnknownType) ^ str ^ "'"))
    | ("Ø" | "rien") :: [] -> `None
    | "?" :: [] -> `Any
    | _ -> raise_name_error ((get_string UnknownType) ^ str ^ "'")

  let get_iterable_type : t -> t option = function
      `String -> Some `Char
    | `List t -> Some t
    | _ -> None

  let rec is_compatible (type1: t) (type2: t) =
    type2 = `Any || match type1 with
    | `Any -> true
    | `Int | `Float | `Char | `String | `Bool | `None -> type1 = type2
    | `List t1 -> (match type2 with
        | `List t2 -> is_compatible t1 t2
        | _ -> false)
    | `Function (params1, return1) -> (match type2 with
        | `Function (params2, return2) -> List.for_all2 is_compatible params1 params2 && is_compatible return1 return2
        | _ -> false)
    | `Custom (name,_,_) -> name = (to_string type2) 

  let get_attr_meths name vars = 
    match (StringMap.find name vars) with 
        |`Custom (_ , attr_meths, are_set) -> attr_meths, are_set
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
(* The current context of the code *)

type context = {
  code: string;                    (* The whole code *)
  index: int;                      (* The index *)
  vars: Type.t StringMap.t;        (* The set of known variables *)
  scopes: scope list;              (* The stack of scopes *)
}

let rec get_current_class_name context = 
    match context.scopes with 
        |[] -> (print_int context.index; print_string "\n"; 
          failwith "Internal error: get_current_class_name is not used with a class context")
        |(Class_def name)::_ -> name 
        |_::r -> get_current_class_name {context with scopes = r} 

let print_vars = StringMap.iter (function name -> function t ->
    print_endline ("var " ^ name ^ " : " ^ Type.to_string t))


module Value = struct

  type t =
    | Int of int
    | Float of float
    | Char of char
    | String of string
    | Bool of bool
    | Variable of string
    | Instance of string *  string
    | None

  let to_string = function
      Int i -> string_of_int i
    | Float f -> string_of_float f
    | Char c -> "'" ^ String.make 1 c ^ "'"
    | String s -> {|"|} ^ s ^ {|"|}
    | Bool b -> if b then "True" else "False"
    | Variable name -> name
    | Instance (type_def, name) -> type_def^"."^name
    | None -> "None"

  let of_string = function str ->
    let str = String.trim str
    and name_re = "[_a-zA-Z][_a-zA-Z0-9]*" in
    if Str.string_match (Str.regexp "^-?[0-9]+$") str 0 then
      Int (int_of_string str)
    else if Str.string_match (Str.regexp {|^[0-9]+.[0-9]*$|}) str 0 then
      Float (float_of_string str)
    else if Str.string_match (Str.regexp {|^'\(.\)'$|}) str 0 then
      Char (Str.matched_group 1 str).[0]
    else if Str.string_match (Str.regexp {|^"\(.*\)"$|}) str 0 then
      String (Str.matched_group 1 str)
    else if str = "vrai" || str = "faux" then
      Bool (str = "vrai")
    else if Str.string_match (Str.regexp ("^" ^ name_re ^ "$")) str 0 then
      Variable str
    else if Str.string_match (Str.regexp ("^instance "^name_re ^ "$")) str 0 then
      Instance ("self", String.sub str 9 (String.length str - 9))
    else if str = "" then
      raise_syntax_error (get_string ExpectedOperand)
    else
      raise_syntax_error (get_string InvalidExpression)

  let get_type context = function
    | Int _ -> `Int
    | Float _ -> `Float
    | Char _ -> `Char
    | String _ -> `String
    | Variable name -> (try StringMap.find name context.vars
                        with Not_found -> raise_name_error ((get_string UnknownVariable) ^ name ^ "'"))
    | Bool _ -> `Bool
    | Instance (type_name, name) -> let type_name = if type_name = "self" then get_current_class_name context else type_name in 
                                    let attr_meths, are_set = try Type.get_attr_meths type_name context.vars 
                                    with Not_found -> raise_name_error ((get_string UnknownVariable) ^type_name ^ "'") in 
                                    let is_set  = try StringMap.find name are_set
                                    with Not_found -> raise_name_error ((get_string UnknownVariable) ^ name^"' in class '"^type_name^"'") in 
                                    if is_set then StringMap.find name attr_meths 
                                    else raise_name_error ("The variable "^name^" has no value in the current context")                                     
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

