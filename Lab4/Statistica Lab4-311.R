#Statistica Lab 4 311

#II.MATRICE

(matrice <- matrix(1:10,nrow=5,ncol=4))
#Q: ce se intampla daca nrow sau ncol nu corespund numarului de valori?
matrice[,2]
#Q: Selectati coloana 1 si liniile 2 si 5 din matrice

#pentru a uni doua linii sau doua coloane intr-o matrice:
nume<-c(letters[1:12])
salarii <- sample(2000:10000,12)
(tabel1 <- cbind(nume,salarii))
(tabel2 <- rbind(nume,salarii))
class(tabel1)
class(tabel2)

#creare obiecte multidimensionale
(cub <- array(1:12, dim = c(3,2,2)))

#-------------------------------------------------------------------------------------------------------------------------

#III. LISTE
#Operatorii [[]] si $
lista <- list(a=10:20, b="text", c(TRUE, FALSE, TRUE), 2+1i)
length(lista)
lista[[1]][2]
lista$a[5]
class(lista)           
class(lista$a)           
class(lista[[4]])

#-------------------------------------------------------------------------------------------------------------------------
#IV. DATAFRAMES

rand1<-letters[6:10]
rand2<-1:5
rand3<-c("text1","text2","text3","text4","text5")
(textul_meu <- data.frame(rand1,rand2,rand3))
#Functiile unlist si lapply vor fi studiate pe larg in laboratoarele urmatoare
c <- unlist(lapply(textul_meu,class))
class(c)
datasets::mtcars
unlist(lapply(mtcars,class))

#stergerea sau adaugarea
dataframe1<-mtcars
dataframe1<-dataframe1[,-c(2:10)]
dataframe1$coloana_noua<-c(1:32)

#-------------------------------------------------------------------------------------------------------------------------
#V.FACTORS

luni <- c("Apr","Mar","Feb","Ian", "Noi","Oct","Dec","Apr", "Mai","Sep","Aug","Ian","Mai", "Iul","Iun","Dec", "Iun","Aug")
fluni <- as.factor(luni) 

#afisarea datelor in tabel
table(fluni)

#pentru ordonarea normala se va utiliza LEVELS si ordered=TRUE
fluni <- factor(luni, levels = c("Ian","Feb", "Mar","Apr", "Mai", "Iun","Iul","Aug", "Sep","Oct", "Noi","Dec"), ordered = TRUE) 
fluni
table(fluni)

#pentru a crea subcategorii, se utilizeaza CUT si BREAKES

salarii <- c(1000,1500,1200,1300,1800,2000,1300,1400,1600,2200,1800,2100)
#Q: Ce face urmatoarea secventa de cod?
fsalarii <- cut(salarii, breaks = c(1500,1750,2000,2500),include.lowest = TRUE,dig.lab = 4 )
table(fsalarii)

#Aplicatii(folosim pachetul prob)
#1. Aruncarea cu moneda
library(prob)

t3 <- tosscoin(3)
str(t3)
t3$toss2
t3[1,]
t3[,1]
#Se arunca o moneda de 3 ori si vrem probabilitatea de aparitie cel putin de 2 ori a H

#Se arunca o moneda o data si vrem probabilitatea de aparitie a H
omega1 <- tosscoin(1)
sum(omega1=='H')/nrow(omega1)

#Revenim la problema cu 3 aruncari
omega3 <- tosscoin(3)
sum(rowSums(omega3=='H')>=2)/nrow(omega3)

#Calculati probabilitatea de aparitie a secventei TT la aruncarea monedei de 3 ori

probTT31 <- (sum((omega3[,1] == 'T') & (omega3[,2] == 'T')) + sum((omega3[,2] == 'T') & (omega3[,3] == 'T')))/ nrow(omega3)

#Q: Calculati probabilitatea de aparitie a secventei TT la aruncarea monedei de 5 ori


#Probabiliatea ca din 3 aruncari sa apara capul cel putin o data
#Q: Comentati urmatoarele alternative de cod: pe care o preferati si de ce?

omega3 <- tosscoin(3)
sum(omega3=="H")/nrow(omega3)
#SAU
sum(omega3[,1]=="H"|omega3[,2]=="H"|omega3[,3]=="H")/nrow(omega3)
#Alta varianta
sum(rowSums(omega3=='H')>0)/nrow(omega3)

#Arunc o moneda de 5 ori si vreau probabilitatea sa obtin cap de 3 ori
omega5 <- tosscoin(5)
sum(rowSums(omega5=="H")==3)/nrow(omega5)

#Arunc o moneda de 3 ori si vreau probabilitatea de a obtine secventa HH
omega3$toss1
omega3[,1]
omega3[6:8,1:2]
omega3[,-2]
o12 <- omega3[(omega3['toss1']=='H')&(omega3['toss2']=='H'),]
o23 <- omega3[(omega3['toss2']=='H')&(omega3['toss3']=='H'),]

#Determinati probabilitatea ca din 7 aruncari sa obtin 5 H
omega7 <- tosscoin(7)
sum(rowSums(omega7=='H')==5)/nrow(omega7)

#Q: Determinati probabilitatea ca din 7 aruncari sa obtinem secventa HH

#Aruncarea unui zar
zar2 <- rolldie(2)
str(zar2)
# Determinati probabilitatea ca la aruncarea a doua zaruri sa se obtina suma 7
#Q: Comentati urmatoarele alternative de cod: pe care o preferati si de ce?
sum(zar2[,1]+zar2[,2]==7)
#SAU
h <- sum(rowSums(zar2)==7)/nrow(zar2)

#Alta varianta
t <- table(rowSums(zar2))/nrow(zar2)
str(t)
t[['7']]

#Jocuri de carti
s <- cards()
str(s)
# Determinati probabilitatea de a extrage o carte de Inima

#Q: Comentati urmatoarele alternative de cod: pe care o preferati si de ce?

sum(s['suit']=='Heart')/nrow(s)
#SAU
(table(s["suit"])/nrow(s))[["Heart"]]



##############################################################
#Functiile cbind, rbind
a <- 1:3
b <- 4:6
c <- cbind(a,b)
d <- rbind(a,b)
##############################################################
obis <- rbind(o12,o23)
obis <- unique(obis)

#Jocul de carti
s <- cards()
str(s)
#Vreau probabilitatea de a extrage o carte <7 de inima

####
s[('rank'=='7')&('suit'=='Heart'),]
s[(s$rank==7),]
p <- s[(s['rank'] == '7') & (s['suit'] == "Heart"),]
nrow(s[((s['rank']) ==  7) & (s['suit'] == "Heart"),]) / nrow(s)


#Lucru cu evenimente:
#union(A,B)-reuniune
#intersect(A,B)-intersectia
#setdiff(A,B)-diferenta

#Vreau sa selectez din omega5 randurile 3 si 7 si coloanele 1,3,4
omega5[c(3,7),c(1,3,4)]
omega5[omega5$toss1=='H',]
omega5[(omega5['toss1']=='H')&(omega5['toss2']=='H'),]

#Q:Determinati probabilitatea ca din 7 aruncari cel putin 3
#sa fie pare

#Evenimente in R (doar in pachetul prob)
#union(A,B) reuniunea
#intersect(A,B) intersectia
#setdiff(A,B) diferenta

#Calculati probabilitatea ca extragand o carte dintr-un pachet cu 
#52 de carti de joc sa obtinem o valoare >7 si culoare "Spade"
#Reprezentati intai cu evenimente
#Calculati probabilitatea

o['rank'==7]
o[o['rank']==7,]

#Problema
card <- cards()
A <- card[card['rank'] == '7' |
            card['rank'] == '8' |
            card['rank'] == '9' |
            card['rank'] == '10' |
            card['rank'] == 'J' |
            card['rank'] == 'Q' |
            card['rank'] == 'K' |
            card['rank'] == 'A',]

B <- card[card['suit'] == 'Spade',]
favorabile <- nrow(intersect(A, B))
totale <- nrow(card)
probabilitate <- favorabile / totale

#Tema:
# 1) Folosind pachetul prob creati obiectul moneda5 ce contine toate rezultatele posibile pe care le putem obtine
#la aruncarea succesiva de 5 ori a unei monede. Folosind selectia intr-un dataframe determinati urmatoarele
#probabilitati:
#a)Aparitia secventei HHTHH
#b)Aparitia secventei THHHT
#c)Numarul de aparitii "H" sa fie mai mare ca numarul de aparitii "T"

# 2) Folositi o structura de date adecvata pentru a stoca informatii despre o v.a. discreta ce ia valorile a1,...an
#(fixati voi n si valorile a1,a2...an astfel incat sa fie ordonate crescator) si probabilitatile p1,p2...pn(le generati voi)
#a)Calculati intr-o maniera eficienta P(a<X<b) unde a si b sunt definite in prealabil
#b)Calculati intr-o maniera eficienta P(a<=X<b|X<=c) unde a, b si c sunt definite in prealabil
