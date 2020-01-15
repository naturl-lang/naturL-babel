type type_struct =
    Int
  | Float
  | Char
  | String
  | Boolean
  | List of type_struct
  | Function of type_struct list * type_struct ;;


type scope =
    Normal
  | Current (* Used to indicate no change of the current scope*)
  | Previous
  | ForDefinition
  | WhileDefinition
  | IfDefinition
  | FuncDefinition
  | FuncHeader
  | FuncBody
