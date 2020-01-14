open Structures

module StringMap = Map.Make(String)

(* Translates a list of couples to a map*)
let _make_map couples =
  let rec __make_map map = function
      [] -> map
    | (a, b) :: t -> __make_map (StringMap.add a b map) t
  in __make_map StringMap.empty couples

type keyword_attrs = {
  translation: string;
  scope: scope;
  terminators: char list
}

let make_keyword_attrs translation ?(terminators = []) scope = {
  translation = translation;
  scope = scope;
  terminators = terminators
}

let keywords = _make_map [
    "function", make_keyword_attrs "def" FuncDefinition ~terminators: ['\n'; '\t'];
    "variables", make_keyword_attrs "" FuncHeader;
    "debut", make_keyword_attrs "" FuncBody;
    "fin", make_keyword_attrs "" Previous;
    "pour", make_keyword_attrs "for" Normal;
    "tant_que", make_keyword_attrs "while" Normal;
    "si", make_keyword_attrs "" Normal;
    "alors", make_keyword_attrs ":" Normal;
    "faire", make_keyword_attrs ":" Normal
  ]
