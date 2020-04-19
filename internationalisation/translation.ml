open Yojson

type language =
  | French
  | English

let lang = ref English

let setLang l =
  lang := l

let set_lang_of_string = function
  | "french" -> setLang French
  | "english" -> setLang English
  | _ -> failwith "Unknown language"

type key =
  | SyntaxError
  | TypeError
  | NameError
  | NameTypeMessage
  | HasTypeMessage
  | ButGotMessage


let json = ref (Basic.from_file "internationalisation/translation.json")


let getLangID = function
  | French -> "fr"
  | English -> "en"

let get_member_from_JSON value =
  let high_member = Yojson.Basic.Util.member value !json in
  Yojson.Basic.Util.to_string (Yojson.Basic.Util.member (getLangID !lang) high_member)

let get_string key  = match key with
  | SyntaxError -> get_member_from_JSON "SyntaxError"
  | TypeError -> get_member_from_JSON "TypeError"
  | NameError -> get_member_from_JSON "NameError"
  | NameTypeMessage -> get_member_from_JSON "NameTypeMessage"
  | HasTypeMessage -> get_member_from_JSON "HasTypeMessage"
  | ButGotMessage -> get_member_from_JSON "ButGotMessage"
