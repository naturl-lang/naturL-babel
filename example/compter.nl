fonction compter(chaine str, caractere ch) -> entier
 variables
   entier compte
   caractere c
debut
  compte <- 0
  pour_chaque c dans str faire
    si c = ch alors
      compte <- compte + 1
    fin
  fin
  retourner compte
fin
