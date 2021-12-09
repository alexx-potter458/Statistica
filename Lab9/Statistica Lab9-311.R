# Statistica Lab9-311

#Lucru cu metoda respingerii
#Reamintim repartitia uniforma pe (a,b)
#f(x)=1/(b-a), x in intervalul (a,b)
a <- 0
b <- 1
t <- seq(-3,3,0.001)
plot(t,dunif(t,a,b),col="red")
a <- -1
b <- 1
plot(t,dunif(t,a,b),col="red")

#Aplicam metoda respingerii pentru a genera o valoare din X cu f(x)=e^x/e-1, 0<=x<=1
#Vrem si un contor care sa numere de cate iteratii a fost nevoie pentru a genera un x
f_resp <- function(v=1)
{#contor
  k <- 1
  u1 <- runif(1)
  u2 <- runif(1)
  while(u2>exp(u1-1)){
    u1 <- runif(1)
    u2 <- runif(1)
    k <- k+1
  }
  x <- u1
  return(c(x,k))
}

f_resp()

#Vrem o functie care genereaza n valori din X
n <- 10^6
f_resp_n <- function(n)
{
  y <- sapply(1:n,f_resp) 
  y
}
m <- f_resp_n(n)
x <- m[1,]
k <- m[2,]
hist(x,freq=F,col="magenta")
f_dens <- function(x)
{
  exp(x)/(exp(1)-1)
}
t <- seq(0,1,0.001)
lines(t,f_dens(t),col="blue")

#media vectorului de contoare
medie <- mean(k)
exp(1)/(exp(1)-1)




#Generam o valoare din repartitia Beta(2,4)
fbeta <- function()
{
  #ok este o "variabila semafor" pentru iesirea din while
  ok <- F
  #k imi numara de cate iteratii e nevoie pentru generarea lui x
  k <- 0
  while(ok==F)
  {
    u <- runif(2)
    #echivalent cu u1 <- runif(1) si u2 <- runif(1)
    if(u[2]<=256/27*u[1]*(1-u[1])^3) {
      x <- u[1]
      ok <- T
    }
    k <- k+1
  }  
  return(c(x,k)) 
}

fbeta()

fbeta_n <- function(n)
{
  x <- c()
  k <- c()
  for(i in 1:n) {
    a <- fbeta()
    x <-c(x,a[1])
    k <- c(k,a[2])
  } 
  
  #Putem face ca la fbeta sa returnam concatenarea dintre x si k dar
  #trebuie sa avem grija la indici
  return(c(x,k))
}

fbeta_n(4)
#O imbunatatire ar fi sa concatenam x si k intr-o forma matriceala
# Functiile rbind() si cbind()

v1 <- 1:3
v2 <- 4:6
m1 <- rbind(v1,v2)
m2 <- cbind(v1,v2)

#vreau sa extrag prima linie
m1[1,]

#prima linie si coloanele 1 si 3
m1[1,c(1,3)]
#sau 
m1[1,-2]

m1[1,-c(1,2)]

#################################################

#Revenim la simulare

fbeta_n_nou <- function(n)
{
  x <- c()
  k <- c()
  for(i in 1:n) {
    a <- fbeta()
    x <-c(x,a[1])
    k <- c(k,a[2])
  } 
  
  #Putem face ca la fbeta sa returnam concatenarea dintre x si k dar
  #trebuie sa avem grija la indici
  return(rbind(x,k))
}

#Valori
m <- fbeta_n_nou(10^4)
x <- m[1,]
k <- m[2,]
medie_incercari <- sum(k)/10^4
t <- seq(0,1,0.001)
hist(x,freq=F,col="blue")
lines(t,dbeta(t,2,4),col="red")

y <- rbeta(10^4,2,4)

hist(x,freq=F,col="blue")
hist(y,freq=F,col="green",add=T)
lines(t,dbeta(t,2,4),col="red")

hist(y,freq=F,col="green")
hist(x,freq=F,col="blue",add=T)
lines(t,dbeta(t,2,4),col="red")

#Scrieti o functie care genereaza o valoare din repartitia de mai sus
#f(x)=10^6/336*x*(1-x)^3
f_respingere <-function()
{
  #folosim variabila booleana ok ca "variabila semafor" pentru iesirea din bucla
  ok <- F
  #k este contorul pentru nr de iteratii necesare pentru generarea lui x
  k <- 0
  while(ok==F)
  {
    y <- runif(1,0.8,1)
    u <- runif(1)
    if(u<=625/4*y*(1-y)^3) {
      x <- y
      ok <- T
    }
    k <- k+1
  }
  
  return(c(x,k))
}
f_respingere()


#Sa scrie o functie care genereaza n valori din repartitia de mai sus

f_respingere_n <- function(n)
{ 
  x <- c()
  k <- c()
  for(i in 1:n) {
    a <- f_respingere()
    x <- c(x,a[1])
    k <- c(k,a[2])
  }
  #vom concatena cu functia rbind()
  return(rbind(x,k))
}

m <- f_respingere_n(10^4)

m[,1:5]
x <- m[1,]
k <- m[2,]
medie_incercari <- sum(k)/10^4
hist(x)

#Sa se scrie o functie care se comporta ca densitatea de mai sus
f1 <- function(x)
{
  10^6/336*x*(1-x)^3
}
t <- seq(0.8,1,0.001)
hist(x,freq=F,col="green")
lines(t,f1(t),col="red")

#TEMA: Rescrieti functia de mai sus astfel incat sa nu se mai bazeze pe apelul 
#functiei f_respingere

#TEMA: Rescrieti functia fbeta_n astfel incat sa genereze n valori
#folosind un algoritm care nu mai apeleaza functia fbeta

#Tema: FOlositi metoda respingerii pentru a genera n valori dintr-o v.a. discreta
# data de valorile -5:4 cu probabilitatile (1/50,2/50,10/50,11/50,9/50,1/50,1/50,3/50,3/50,3/50)