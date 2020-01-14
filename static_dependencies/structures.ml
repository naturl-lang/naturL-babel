type position = {row: int; column: int}

type type_struct =
    Int
  | Float
  | Char
  | String
  | List of type_struct
  | Function of type_struct list * type_struct ;;


type scope =
    Normal
  | Previous
  | FuncDefinition
  | FuncHeader
  | FuncBody
