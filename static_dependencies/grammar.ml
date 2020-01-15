open Structures
    
module StringMap = Map.Make(String)
    
(* Converts a list of couples to a map*)
let _make_map couples =
  let rec __make_map map = function
      [] -> map
    | (a, b) :: t -> __make_map (StringMap.add a b map) t
  in __make_map StringMap.empty couples


(*********** KEYWORDS ***********)

type keyword_attrs = {
  translation: string;
  scope: scope;
  terminators: string list;
  valid_scopes: scope list  (* The list of scopes in which the keyword can be encountered *)
}

let make_keyword_attrs translation ?(terminators = []) ?(valid_scopes = [FuncBody; Normal]) scope = {
  translation = translation;
  scope = scope;
  terminators = terminators;
  valid_scopes = valid_scopes
}

let keywords = _make_map [
    "function", make_keyword_attrs "def" FuncDefinition ~terminators: ["\n"; "\r"];
    "variables", make_keyword_attrs "" Current ~valid_scopes: [FuncHeader];
    "debut", make_keyword_attrs "" FuncBody ~valid_scopes: [FuncHeader];
    "fin", make_keyword_attrs "" Previous ~terminators: ["\n"; "\r"];
    "pour", make_keyword_attrs "for" ForHeader ~terminators: ["\n"; "\r"];
    "tant_que", make_keyword_attrs "while" Normal;
    "si", make_keyword_attrs "if" IfDefinition ~terminators: ["\n"; "\r"];
    "sinon", make_keyword_attrs "else:" Current ~terminators: ["\n"; "\r"];
    "sinon_si", make_keyword_attrs "elif" IfDefinition ~terminators: ["\n"; "\r"];
    "alors", make_keyword_attrs ":" Current ~valid_scopes: [IfDefinition];
    "faire", make_keyword_attrs ":" Current ~valid_scopes: [WhileDefinition; ForDefinition]
  ]
