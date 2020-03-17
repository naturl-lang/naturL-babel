open Dependencies.Tokenizer;;


let%expect_test "tokenizer1" =
  let tokens = tokenize "2 + 'a' + \"\"Ahah\"\" - fonction et vrai ou Faux + compter(2, 3) ou L[2]"  in
  print_tokens tokens;
  [%expect {|
Litteral 2
Operator +
Litteral 'a'
Operator +
Litteral ""Ahah""
Operator -
Identifier fonction
Operator et
Litteral vrai
Operator ou
Identifier Faux
Operator +
Identifier compter
OpenP
Litteral 2
Coma
Litteral 3
CloseP
Operator ou
Identifier L
OpenHook
Litteral 2
CloseHook |}];;

let%expect_test "tokenizer2" =
    let tokens = tokenize "a et b ou c et d + \"aled\" <= 2 >= 4 > 2 < 7 ou fonction(2, 4, valeur) = 2.687 ou 3.14159"  in
    print_tokens tokens;
    [%expect {|
Identifier a
Operator et
Identifier b
Operator ou
Identifier c
Operator et
Identifier d
Operator +
Litteral "aled"
Operator <=
Litteral 2
Operator >=
Litteral 4
Operator >
Litteral 2
Operator <
Litteral 7
Operator ou
Identifier fonction
OpenP
Litteral 2
Coma
Litteral 4
Coma
Identifier valeur
CloseP
Operator =
Litteral 2.687
Operator ou
Litteral 3.14159 |}];;
