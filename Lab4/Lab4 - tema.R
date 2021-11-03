# 1) Folosind pachetul prob creati obiectul moneda5 ce contine toate rezultatele posibile pe care le putem obtine
#la aruncarea succesiva de 5 ori a unei monede. Folosind selectia intr-un dataframe determinati urmatoarele
#probabilitati:
#a)Aparitia secventei HHTHH
#b)Aparitia secventei THHHT
#c)Numarul de aparitii "H" sa fie mai mare ca numarul de aparitii "T"

library(prob)
moneda5 <- data.frame(tosscoin(5))

#a
hhthh <- sum(moneda5['toss1']=="H" & moneda5['toss2']=="H" & moneda5['toss3']=="T" & moneda5['toss4']=="H" & moneda5['toss5']=="H")/nrow(moneda5)
#b
hhthh <- sum(moneda5['toss1']=="T" & moneda5['toss2']=="H" & moneda5['toss3']=="H" & moneda5['toss4']=="H" & moneda5['toss5']=="T")/nrow(moneda5)
#c
ht <-  sum(moneda5 == "H")/(sum(moneda5 == "H" | moneda5 == "T"))

# 2) Folositi o structura de date adecvata pentru a stoca informatii despre o v.a. discreta ce ia valorile a1,...an
#(fixati voi n si valorile a1,a2...an astfel incat sa fie ordonate crescator) si probabilitatile p1,p2...pn(le generati voi)
#a)Calculati intr-o maniera eficienta P(a<X<b) unde a si b sunt definite in prealabil
#b)Calculati intr-o maniera eficienta P(a<=X<b|X<=c) unde a, b si c sunt definite in prealabil

n <-  10
an <- 1:n
pn <- sample(seq(0,1,length.out = n))
pn <- pn/sum(pn)
probtable <-data.frame(an, pn)

#a
a <- 3
b <- 8
probtableA <- (probtable[(probtable$an>a) & (probtable$an<b),])
sum(probtableA$pn)

#b
a <- 5
b <- 9
c <- 2
probtableB <- (probtable[((probtable$an>=a) & (probtable$an<b)) | (probtable$an<=c),])
sum(probtableB$pn)
