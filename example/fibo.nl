fonction fibo(entier n) -> entier
 variables
   entier i
   entier a, b, c
debut
  a <- 0
  b <- 1
  pour i de 1 jusqu_a n faire
    c <- a
    a <- b
    b <- b + c
  fin
  retourner a
fin
