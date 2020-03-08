fonction majeur(entier age) -> chaine
debut
  si age < 0 ou age >= 120 alors
    retourner "menteur"
  sinon_si age = 18 alors
    retourner "à peine majeur"
  sinon_si age > 18 alors
    retourner "majeur"
  sinon
    retourner "mineur"
  fin
fin
