(* Unparametrized types *)
module Type =
struct
  type type_struct =
    | Int
    | Float
    | Char
    | String
    | Boolean
    | List
    | Function
  let int = Int
  let float = Float
  let char = Char
  let str = String
  let bool = Boolean
  let list = List
  let func = Function
end


type variable = {name: string; _type: Type.type_struct}

(* Parametrized types *)
type type_struct =
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Boolean of bool
  | List of type_struct
  | Function of type_struct list * type_struct
  | Variable of variable


type expr =
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Equal of expr * expr
  | Greater of expr * expr
  | GreaterOrEqual of expr * expr
  | Lower of expr * expr
  | LowerOrEqual of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Value of type_struct

type 'a tree =
  | Leaf of 'a
  | Tree of 'a * 'a tree list
