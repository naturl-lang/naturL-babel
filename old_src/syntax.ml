let keywords = [
  "fonction";
  "procedure";
  "si";
  "sinon";
  "sinon_si";
  "pour";
  "pour_chaque";
  "tant_que";
  "alors";
  "faire";
  "de";
  "dans";
  "jusqu_a";
  "debut";
  "fin";
  "retourner";
  "variables";
  "type_abstrait";
  "methodes";
  "attributs";
]


let resolve_py_conficts name =
  let py_keywords = [
    "False";
    "True";
    "None";
    "and";
    "or";
    "not";
    "as";
    "assert";
    "async";
    "await";
    "break";
    "class";
    "continue";
    "def";
    "del";
    "elif";
    "else";
    "except";
    "finally";
    "for";
    "from";
    "global";
    "if";
    "import";
    "in";
    "is";
    "lambda";
    "nonlocal";
    "pass";
    "raise";
    "return";
    "try";
    "while";
    "with";
    "yield"
  ]
  in
  let is_py_keyword name =
    py_keywords |> List.mem name
  in
  let clean_string str =
    let cleaned = Str.replace_first (Str.regexp "^\\(.*\\)_*$") "\\1" str in
    cleaned
  in
  if is_py_keyword (clean_string name) then
    name ^ "_"
  else
    name
