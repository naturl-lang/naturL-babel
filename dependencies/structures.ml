type type_struct =
  | Int
  | Float
  | Char
  | String
  | Boolean
  | List of type_struct
  | Function of type_struct list * type_struct ;;

type expr =
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Value of type_struct

type 'a tree =
  | Leaf of 'a
  | Tree of 'a * 'a tree list
