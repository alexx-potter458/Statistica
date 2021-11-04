# Statistica Lab5 311
#1. Lucru cu functii in R

f <- function()
{
  #optional return()
}

#Stocam expresii matematice
f1 <- function(x)
{
  x^2/3
}
f1(3)
#Fenomen ciudat
f11 <- function(x)
{
  x <- x^2/3
}
f11(3)
y <- f11(3)
#Q: Care credeti ca este explicatia fenomenului de mai sus?

#Functia integrate

a <- integrate(f1,0,1)
a$value
# Functia integrate lucrareaza si cu capete infinite, dar
#numai daca integrala e convergenta
integrate(f1,0,Inf)

fgama <- function(x,a)
{
  x^(a-1)*exp(-x)
}

integrate(fgama,0,Inf,a=7)
factorial(6)

#2. Intructiuni de control 

#2.1. IF...ELSE

#if(conditie)
#{
#  bloc de executat
#}

#sau
#if(conditie)
#{
#  bloc de executat
#} else {                    #ATENTIE: Else trebuie sa fie aici!
#        bloc alternativ
#       }


#Q: Generati un vector cu 100 de valori naturale. Daca pentru o pozitie impara valoarea din vector e para afisati "DA",
#altfel afisati "NU"


#2.2. ifelse()
#Functia ifelse() returneaza un vector de valori alese in functie de o conditie
#Versiune "vectorizata" a lui if...else
#Similara cu operatorul ?: din C

#Q: Explicati urmatoarea secventa de cod 
test <- (1:10)%%2==T
yes <- rep("DA",10)
no <- rep("NU",10)
ret <- ifelse(test, yes, no)

#Folosim ifelse pentru unele validari
x <- 5:-3
sqrt(x) #Warning
sqrt(ifelse(x>=0,x,NA))
#Q: De ce ifelse(x>=0,sqrt(x),NA) produce un warning? 

#2.3. For
# for( valori IN vector)
#{
#bloc de executat
#}

for (i in 1:7)
{
  print(i^2)
}

#2.4 While 

# while(conditie)
#{
# bloc de executat
#}

#Determinati puterea cea mai mare a lui 2 mai mica decat 100
i <- 0
while(2^i<100){
  i <- i+1
}
print(c(i-1,2^(i-1)))
#Q: De ce folosesc i-1 si nu i?

#2.5. Break si next
# Cu break se iese din executia unui ciclu repetitiv
# Controlul este redat primei instructiuni din afara buclei curente
# Next sare din iteratia curenta catre urmatoarea
# In cazul buclelor imbricate atat break cat si next se aplica
# doar buclei curente

#Determinati suma numerelor pare de la 1 la 10
i <- 0
sumEven <- 0
while(i<=10){
  i <- i+1
  if(i%%2==1)
    next
  print(i)
  sumEven <- sumEven+i
}

#2.6. Repeat
# repeat
#{
#  bloc de executat
#}
#Executia se opreste cu break

#Q: Ce face urmatoarea secventa de cod?
i <- 0
repeat {
  if(2^(i+1)>=110)
    break
    i <- i+1
}
print(c(i,2^i))

# 2.7. Eficientizarea codului; Functia system.time
n <- 10^6
a1 <- numeric(n)
system.time({for(i in 1:n) a1[i] <- (1+1/i)^i})
system.time({a2 <- (1+1/(1:n))^(1:n)})
# Ne uitam pe coloana user care ne da timpul real de procesare exprimat in secunde

# 2.8. Functia replicate()
# Aceasta functie repeta de mai multe ori o secventa si intoarce un vector sau o matrice

#replicate(numar, { bloc de executat})

results <- replicate(50,
                     {
                       esantion <- sample(1:10,5)
                       sd(esantion)
                       })

#3. Reprezentarea grafica a functiilor
#Discretizarea intervalului pe care vrem sa reprezentam grafic functia
t <- seq(-5,8,0.001)
plot(t,f1(t),col="magenta")
# Sa se reprezinte grafic sirul de functii fn(x)=x^n pe [0,1]
t <- seq(0,1,0.001)
plot(t,t,col="magenta")
for (i in 2:7)lines(t,t^i,col=i)


#TEMA:

#1) Creati o functie in R numita gama_nume care sa implementeze proprietatile
#pe care le are functia gama(vezi documentul Integrale euleriene) si sa 
#foloseasca apelul functiei integrate doar atunci cand parametrul nu satisface 
#nicio conditie "buna"

# gama_nume <- function(....)
#{
#daca n e natural atunci foloseste propr. 3)     #folosim functia din R numita factorial
#daca n e de forma b/2(cu b natural) foloseste formula 2) si 4)
#altfel foloseste formula 2) pana cand argumentul devine subunitar
#si doar pentru acea valoare calculeaza cu integrate

#2) Implementati o functie care calculeaza beta(a,b) folosindu-va de proprietati
#si de integrala gama
