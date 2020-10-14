let translation = {|{
	"SyntaxError": {
		"fr": "Erreur de Syntaxe",
		"en": "Syntax Error"
	},
	"TypeError": {
		"fr": "Erreur de Type",
		"en": "Type Error"
	},
	"NameError": {
		"fr": "Erreur de nommage",
		"en": "Name error"
	},
	"ImportError":{
		"fr": "Erreur d'importation",
		"en": "Import Error"
	},
	"Warning":{
		"fr": "Avertissement à la ligne ",
		"en": "Warning at line "
	},
	"NameTypeMessage": {
		"fr": "Une expression de type '",
		"en": "Expected an expression of type '"
	},
	"NameButGotMessage": {
		"fr": "' était attendue mais cette expression est de type '",
		"en": "' but got '"
	},
	"HasTypeMessage": {
		"fr": "' a le type '",
		"en": "' has type '"
	},
	"ButGotMessage": {
		"fr": "' mais le type affecté est '",
		"en": "' but got '"
	},
	"ReturnTypeMatchMessage": {
		"fr": "La type de retour n'est pas le même que celui de la fonction",
		"en": "The return type does not match the function type"
	},
	"UnexpectedReturn": {
		"fr": "Instruction 'retouner' non attendue ",
		"en": "Unexpected instruction 'retourner' "
	},
	"UnexpectedToken": {
		"fr": "Token inconnu '",
		"en": "Unexpected token '"
	},
	"UnexpectedEOF": {
		"fr": "Fin de fichier non attendue",
		"en": "Unexpected end of file"
	},
	"ExpectedDebut": {
		"fr": "Mot clé 'debut' attendu après la définition de la fonction",
		"en": "Expected 'debut' after function definition"
	},
	"UnexpectedDebut": {
		"fr": "Mot clé 'debut' non attendu",
		"en": "Unexpected token 'debut'"
	},
	"UnexpectedFin": {
		"fr": "Mot clé 'fin' non attendu",
		"en": "Unexpected token 'fin' "
	},
	"ExpectedReturn": {
		"fr": "Une fonction doit forcément retourner une valeur",
		"en": "A return value is expected in a function"
	},
	"ExpectedFin": {
		"fr": "Bloc de code non terminé : mot clé 'fin' attendu",
		"en": "Open control structure : 'fin' keyword expected"
	},
	"UnexpectedChar": {
		"fr": "Caractère non attendu '",
		"en": "Unexpected character '"
	},
	"InFunctionDefinition": {
		"fr": "' dans la définition de la fonction",
		"en": "' in function definition"
	},
	"BreakingReturn": {
		"fr": "Retour débranchant",
		"en": "'retourner' inside a loop"
	},
	"AlwaysTrue": {
		"fr": "Cette expression est toujours vraie",
		"en": "This expression is always true"
	},
	"AlwaysFalse": {
		"fr": "Cette expression est toujours fausse",
		"en": "This expression is always false"
	},
	"MissingKeyword": {
		"fr": "Mot clé manquant '",
		"en": "Missing keyword '"
	},
	"InvalidFunctionDefinition": {
		"fr": "Définition de fonction invalide",
		"en": "Invalid function defintion"
	},
	"UnknownVariable": {
		"fr": "Variable inconnue '",
		"en": "Unknwown variable '"
	},
	"UnknownType": {
		"fr": "Type inconnu '",
		"en": "Unknown type '"
	},
	"ExpectedOperand": {
		"fr": "Opérande attendue",
		"en": "Expected operand"
	},
	"InvalidExpression": {
		"fr": "Expression invalide",
		"en": "Invalid expression"
	},
	"InvalidTokenExpression": {
		"fr": "Identificateur invalide dans l'expression: '",
		"en": "Invalid token in expression: '"
	},
	"ReservedKeyword": {
		"fr": "' est un mot clé réservé",
		"en": "' is a reserved keyword"
	},
	"TokenCapture": {
		"fr": "Identificateur inconnu",
		"en": "Could not capture the unknown token"
	},
	"MissingClosingParenthesis": {
		"fr": "Parenthèse fermante manquante : ')'",
		"en": "Missing closing parenthesis : ')'"
	},
	"MissingClosingBracket": {
		"fr": "Crochet fermant manquant : ']'",
		"en": "Missing closing bracket : ']'"
	},
	"UnexpectedParenthesis": {
		"fr": "Identificateur inattendu : ')'",
		"en": "Unexpected token : ')'"
	},
	"UnexpectedBracket": {
		"fr": "Identificateur inattendu : ']'",
		"en": "Unexpected token : ']'"
	},
	"UnknownOperator": {
		"fr": "Opérateur inconnu '",
		"en": "Unknown operator '"
	},
	"InvalidOperation": {
		"fr": "Opération invalide pour les expression de type '",
		"en": "Invalid operation for expressions of type '"
	},
	"AndType": {
		"fr": "' et de type '",
		"en": "' and type '"
	},
	"CannotCompare": {
		"fr": "Impossible de comparer une expression de type ",
		"en": "Cannot compare expressions of type "
	},
	"VariablesOfType": {
		"fr": "Les variables de type ",
		"en": "Variables of type "
	},
	"NotCallable": {
		"fr": " ne sont pas applicables",
		"en": " are not callable"
	},
	"UnknownFunction": {
		"fr": "Fonction ou procédure inconnue: '",
		"en": "Unknown function or procedure: '"
	},
	"TheType": {
		"fr": "Le type '",
		"en": "The type '"
	},
	"NotSubscriptable": {
		"fr": "' n'est pas énumérable",
		"en": "' is not subscriptable"
	},
	"ListIndicesIntegers": {
		"fr": "Les indices d'accès aux listes doivent êtres entiers",
		"en": "List indices must be integers"
	},
	"GivenExpression": {
		"fr": "L'expression founie ne vérifie pas le type déclaré",
		"en": "Given expression does not match the declared type"
	},
	"AttributeScore":{
		"fr": "Le scope de déclaration des attributs n'est jamais fermé",
		"en": "Attributes scope is never closed"
	},
	"DeclareAttributes": {
		"fr": "Impossible de déclarer une méthode sans aucun attributs",
		"en": "Cannot declaire method without any attributes"
	},
	"AllListElements": {
		"fr": "Tous les éléments d'une liste doivent être du même type",
		"en": "All elements of a list must have the same type"
	},
	"AfterLineContinuation": {
		"fr": "' après un caractère de continuation de ligne",
		"en": "' after line continuation character"
	},
	"HasNoAttribute": {
		"fr": "' n'as pas d'attribut ",
		"en": "' has no attribute"
	},
	"UnknownPackage": {
		"fr": "Paquet inconnu '",
		"en": "Unknown package '"
	},
	"MissingNaturlPackage": {
		"fr": " ' (ficher naturl-package manquant)",
		"en": "' (missing naturl-package file)"
	},
	"CannotImportPackage": {
		"fr": "Impossible d'importer le paquet '",
		"en": "Cannot importe package '"
	},
	"UndefinedType": {
		"fr": "Type indéfini '",
		"en": "Undefined type '"
	},
	"InClass": {
		"fr": "' dans le type abstrait '",
		"en": "' in class '"
	},
	"TheVariable": {
		"fr": "La variable '",
		"en": "The variable '"
	},
	"HasNoValue": {
		"fr": "' n'as pas de valeur dans le contexte actuel",
		"en": "' has no value in current context"
	},
	"CannotResolveName": {
		"fr": "Impossible de reconnaître le nom '",
		"en": "Cannot resolve name '"
	},
	"CannotDeclareMethod": {
		"fr": "Impossible de déclarer une méthode sans arguments",
		"en": "Cannot declare a method without arguments"
	},
	"KeywordInstance": {
		"fr": "Le mot clé 'instance' ne peut pas être utilisé en dehors d'une définiton de type abstrait",
		"en": "Keyword 'instance' cannot be used outside a class definition"
	},
	"UnclosedScope": {
		"fr": "Environnement non fermé : mot clé 'fin' attendu",
		"en": "Unclosed scope: expected 'fin'"
	},
	"MaybeSkipLine": {
		"fr": "Mot clé 'fin' non attendu, peut être faut-il sauter une ligne ?",
		"en": "Unexpected keyword 'fin'. Maybe you should skip a line"
	},
	"UnclosedVariable": {
		"fr": "L'environnement de définition de variables n'est pas fermé",
		"en": "'variable' environment is unclosed"
	},
	"ConstructorReturn": {
		"fr": "Le type de retour du constructeur n'est pas le même que celui du type abstrait défini",
		"en": "Constructor return type is not the same as class type"
	},
	"FirstMethod": {
		"fr": "La première méthode d'un type abstrait doit être un constructeur mais la première méthode était :",
		"en": "The first method of a class needs to be a constructor but got :"
	}
}|}

open Yojson

exception NoNaturLPath

let () =
  Printexc.register_printer
    (function
      | NoNaturLPath -> Some (Printf.sprintf ": no NATURLPATH environment variable is set on this computer")
      | _ -> None (* for other exceptions *)
    )

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
  | ImportError
  | Warning
  | NameTypeMessage
  | NameButGotMessage
  | HasTypeMessage
  | ButGotMessage
  | ReturnTypeMatchMessage
  | UnexpectedReturn
  | UnexpectedToken
  | UnexpectedEOF
  | ExpectedDebut
  | UnexpectedDebut
  | UnexpectedFin
  | ExpectedReturn
  | ExpectedFin
  | UnexpectedChar
  | InFunctionDefinition
  | BreakingReturn
  | AlwaysTrue
  | AlwaysFalse
  | MissingKeyword
  | UnknownVariable
  | InvalidFunctionDefinition
  | UnknownType
  | ExpectedOperand
  | InvalidExpression
  | InvalidTokenExpression
  | ReservedKeyword
  | TokenCapture
  | MissingClosingParenthesis
  | MissingClosingBracket
  | UnexpectedParenthesis
  | UnexpectedBracket
  | UnknownOperator
  | InvalidOperation
  | AndType
  | CannotCompare
  | VariablesOfType
  | NotCallable
  | UnknownFunction
  | TheType
  | NotSubscriptable
  | ListIndicesIntegers
  | GivenExpression
  | AttributeScope
  | DeclareAttributes
  | AllListElements
  | AfterLineContinuation
  | HasNoAttribute
  | UnknownPackage
  | CannotImportPackage
  | UndefinedType
  | InClass
  | TheVariable
  | HasNoValue
  | CannotResolveName
  | CannotDeclareMethod
  | KeywordInstance
  | UnclosedScope
  | MaybeSkipLine
  | UnclosedVariable
  | ConstructorReturn
  | FirstMethod
  | MissingNaturlPackage

let to_int = function
  | SyntaxError -> 1
  | TypeError -> 2
  | NameError -> 3
  | ImportError -> 4
  | NameTypeMessage -> 5
  | NameButGotMessage -> 6
  | HasTypeMessage -> 7
  | ButGotMessage -> 8
  | ReturnTypeMatchMessage -> 9
  | UnexpectedReturn -> 10
  | UnexpectedToken -> 11
  | UnexpectedEOF -> 12
  | ExpectedDebut -> 13
  | UnexpectedDebut -> 14
  | UnexpectedFin -> 15
  | ExpectedReturn-> 16
  | ExpectedFin -> 17
  | UnexpectedChar -> 18
  | InFunctionDefinition -> 19
  | BreakingReturn -> 20
  | AlwaysTrue -> 21
  | AlwaysFalse -> 22
  | MissingKeyword -> 23
  | UnknownVariable -> 24
  | InvalidFunctionDefinition ->25
  | UnknownType -> 26
  | ExpectedOperand -> 27
  | InvalidExpression -> 28
  | InvalidTokenExpression -> 29
  | ReservedKeyword -> 30
  | TokenCapture -> 31
  | MissingClosingParenthesis -> 32
  | MissingClosingBracket -> 33
  | UnexpectedParenthesis -> 34
  | UnexpectedBracket -> 35
  | UnknownOperator -> 36
  | InvalidOperation -> 37
  | AndType -> 38
  | CannotCompare -> 39
  | VariablesOfType -> 40
  | NotCallable -> 41
  | UnknownFunction -> 42
  | TheType -> 43
  | NotSubscriptable -> 44
  | ListIndicesIntegers -> 45
  | GivenExpression -> 46
  | AttributeScope -> 47
  | DeclareAttributes -> 48
  | AllListElements -> 49
  | AfterLineContinuation -> 50
  | HasNoAttribute -> 51
  | UnknownPackage -> 52
  | CannotImportPackage -> 53
  | UndefinedType -> 54
  | InClass -> 55
  | TheVariable -> 56
  | HasNoValue -> 57
  | CannotResolveName -> 58
  | CannotDeclareMethod -> 59
  | KeywordInstance -> 60
  | UnclosedScope -> 61
  | MaybeSkipLine -> 62
  | UnclosedVariable -> 63
  | ConstructorReturn -> 64
  | FirstMethod -> 65
  | Warning -> 66
  | MissingNaturlPackage -> 67


let json =
  try
    let path = [Sys.getenv "NATURLPATH"; "internationalisation"; "translation.json"]
               |> List.fold_left (fun s -> fun elt -> Filename.concat s elt) ""
    in
                 ref (Basic.from_file path)
  with Not_found -> ref (Basic.from_string translation)


let getLangID = function
  | French -> "fr"
  | English -> "en"

let get_member_from_JSON value =
  try
    let high_member = Yojson.Basic.Util.member value !json in
    Yojson.Basic.Util.to_string (Yojson.Basic.Util.member (getLangID !lang) high_member)
  with Yojson.Basic.Util.Type_error _ -> failwith ("JSON error with key " ^ value)

let get_string key  =
   (*print_int(to_int key) ;*)
   match key with
  | SyntaxError -> get_member_from_JSON "SyntaxError"
  | TypeError -> get_member_from_JSON "TypeError"
  | NameError -> get_member_from_JSON "NameError"
  | ImportError -> get_member_from_JSON "ImportError"
  | Warning -> get_member_from_JSON "Warning"
  | NameTypeMessage -> get_member_from_JSON "NameTypeMessage"
  | NameButGotMessage -> get_member_from_JSON "NameButGotMessage"
  | HasTypeMessage -> get_member_from_JSON "HasTypeMessage"
  | ButGotMessage -> get_member_from_JSON "ButGotMessage"
  | ReturnTypeMatchMessage -> get_member_from_JSON "ReturnTypeMatchMessage"
  | UnexpectedReturn -> get_member_from_JSON "UnexpectedReturn"
  | UnexpectedToken -> get_member_from_JSON "UnexpectedToken"
  | UnexpectedEOF -> get_member_from_JSON "UnexpectedEOF"
  | ExpectedDebut -> get_member_from_JSON "ExpectedDebut"
  | UnexpectedDebut -> get_member_from_JSON "UnexpectedDebut"
  | UnexpectedFin -> get_member_from_JSON "UnexpectedFin"
  | ExpectedReturn -> get_member_from_JSON "ExpectedReturn"
  | ExpectedFin -> get_member_from_JSON "ExpectedFin"
  | UnexpectedChar -> get_member_from_JSON "UnexpectedChar"
  | InFunctionDefinition -> get_member_from_JSON "InFunctionDefinition"
  | BreakingReturn -> get_member_from_JSON "BreakingReturn"
  | AlwaysTrue -> get_member_from_JSON "AlwaysTrue"
  | AlwaysFalse -> get_member_from_JSON "AlwaysFalse"
  | MissingKeyword -> get_member_from_JSON "MissingKeyword"
  | InvalidFunctionDefinition -> get_member_from_JSON "InvalidFunctionDefinition"
  | UnknownVariable -> get_member_from_JSON "UnknownVariable"
  | UnknownType -> get_member_from_JSON "UnknownType"
  | ExpectedOperand -> get_member_from_JSON "ExpectedOperand"
  | InvalidExpression -> get_member_from_JSON "InvalidExpression"
  | InvalidTokenExpression -> get_member_from_JSON "InvalidTokenExpression"
  | ReservedKeyword -> get_member_from_JSON "ReservedKeyword"
  | TokenCapture -> get_member_from_JSON "TokenCapture"
  | MissingClosingParenthesis -> get_member_from_JSON "MissingClosingParenthesis"
  | MissingClosingBracket -> get_member_from_JSON "MissingClosingBracket"
  | UnexpectedParenthesis -> get_member_from_JSON "UnexpectedParenthesis"
  | UnexpectedBracket -> get_member_from_JSON "UnexpectedBracket"
  | UnknownOperator -> get_member_from_JSON "UnknownOperator"
  | InvalidOperation -> get_member_from_JSON "InvalidOperation"
  | AndType -> get_member_from_JSON "AndType"
  | CannotCompare -> get_member_from_JSON "CannotCompare"
  | VariablesOfType -> get_member_from_JSON "VariablesOfType"
  | NotCallable -> get_member_from_JSON "NotCallable"
  | UnknownFunction -> get_member_from_JSON "UnknownFunction"
  | TheType -> get_member_from_JSON "TheType"
  | NotSubscriptable -> get_member_from_JSON "NotSubscriptable"
  | ListIndicesIntegers -> get_member_from_JSON "ListIndicesIntegers"
  | GivenExpression -> get_member_from_JSON "GivenExpression"
  | AttributeScope -> get_member_from_JSON "AttributeScope"
  | DeclareAttributes -> get_member_from_JSON "DeclareAttributes"
  | AllListElements -> get_member_from_JSON "AllListElements"
  | AfterLineContinuation -> get_member_from_JSON "AfterLineContinuation"
  | HasNoAttribute -> get_member_from_JSON "HasNoAttribute"
  | UnknownPackage -> get_member_from_JSON "UnknownPackage"
  | CannotImportPackage -> get_member_from_JSON "CannotImportPackage"
  | UndefinedType -> get_member_from_JSON "UndefinedType"
  | InClass -> get_member_from_JSON "InClass"
  | TheVariable -> get_member_from_JSON	"TheVariable"
  | HasNoValue -> get_member_from_JSON "HasNoValue"
  | CannotResolveName -> get_member_from_JSON "CannotResolveName"
  | CannotDeclareMethod -> get_member_from_JSON "CannotDeclareMethod"
  | KeywordInstance-> get_member_from_JSON "KeywordInstance"
  | UnclosedScope -> get_member_from_JSON "UnclosedScope"
  | MaybeSkipLine -> get_member_from_JSON "MaybeSkipLine"
  | UnclosedVariable -> get_member_from_JSON "UnclosedVariable"
  | ConstructorReturn -> get_member_from_JSON "ConstructorReturn"
  | FirstMethod -> get_member_from_JSON "FirstMethod"
  | MissingNaturlPackage -> get_member_from_JSON "MissingNaturlPackage"
