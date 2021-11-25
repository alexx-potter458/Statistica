#Statistica Lab8 311

#Eficientizari de cod
#Algoritmul general de simulare de v.a. discrete
prob <- c(1/3, 1/30, 2/15, 7/30, 4/15)
#Pt sortarea vectorului putem folosi sort()
u <- runif(1)
#x <- min(which(cumsum(prob)-u>0))
x <- which(cumsum(prob)-u>0)[1]

#Evitam for cu
#Familia de functii apply: lapply,sapply,tapply 
sapply(list(1:10,2:5,3:9),max)
lapply(list(1:10,2:5,3:9),max)
#Q: Ce diferenta observati intre cele 2 functii?

#Functia replicate
replicate(50,{X <- sample(1:10,5);m <- min(X)} )

#Functia sample
set.seed(15129)
sample(1:100,4)
sample(1:100,400,replace=T)
x <- sample(-1:1,10^6,replace=T,prob=c(1/2,1/3,1/6))
x[1:20]
valminus1 <- length(x[x==-1])/10^6
val0 <- length(x[x==0])/10^6
val1 <- length(x[x==1])/10^6
#sau folosim functia table
u <- table(x)
str(u)
indice_mod <- max(u)
u[u==max(u)]
#Q: cum extrag doar atributul numeric pentru calcularea valorii modale

x1 <- sample(-1:1,10,replace=T,prob=c(1/2,1/3,1/6))
valminus11 <- length(x1[x1==-1])/10
val01 <- length(x1[x1==0])/10
val11 <- length(x1[x1==1])/10

#Q: Ce obtin aici?
sample(1:5,5)

#Repartitii de v.a.
#1.d+nume_repartitie=functie de masa(caz discret)/functia de densitate de probabilitate(caz continuu)
#primul argument reprezinta vectorul de valori in care vrem sa evaluam functia, iar pe urmatoarele pozitii
#sunt parametrii repartitiei, pusi in ordine
#dgeom(x,p)
#dbinom(x,n,p)
#Ce inseamna?
dbinom(3,5,0.4) #Probabilitatea sa reusesc de 3 ori din 5 incercari cu probabilitatea de succes de 0.4
#P(X=3)
plot(0:10,dbinom(0:10,5,0.4), col="red")
lines(0:10,dbinom(0:10,5,0.4), col="red")

plot(0:10,dbinom(0:10,5,0.9), col="red")
lines(0:10,dbinom(0:10,5,0.9), col="red")

plot(0:10,dbinom(0:10,5,0.1), col="red")
lines(0:10,dbinom(0:10,5,0.1), col="red")

plot(0:100,dbinom(0:100,100,0.4), col="red")
lines(0:100,dbinom(0:100,100,0.4), col="red")


#dexp(x,lambda)
dexp(3,1) #NU mai e o probabilitate
t <- seq(0.001,10,0.001)
plot(t,dexp(t,1),ylim=c(0,0.05))
plot(t,dexp(t,5))
lines(t,dexp(t,1/2), col="red")
lines(t,dexp(t,1),col="blue")


#2. p+nume_repartitie=functia de repartitie
##primul argument reprezinta vectorul de valori in care vrem sa evaluam functia, iar pe urmatoarele pozitii
#sunt parametrii repartitiei, pusi in ordine
#  pbinom(x,n,p)
#P(X<=x)
pbinom(3,5,0.4) #Probabilitatea sa obtinem cel mult 3 succese din 5 incercari cu probabilitatea de succes de 0.4
t <- seq(0,8,0.001)
plot(t,pbinom(t,5,0.4))
plot(t,pexp(t,1))
lines(t,pexp(t,1/2), col="red")
lines(t,pexp(t,5),col="blue")


#3. r+nume_repartitie=genereaza valori din acel tip de repartitie
#  rbinom(nr,n,p)
#nr-numarul de valori pe care le vrem generate

rbinom(3,5,0.4) # genereaza 3 valori dintr-o v.a. repartizata binomial cu parametrii 5 si 0.4
y <- rbinom(10^6,5,0.4)
hist(y)

rexp(3,1) # genereaza 3 valori dintr-o v.a. repartizata exponential de parametru 1
y1 <-rexp(10^6,1)
hist(y1,freq=F)
t <- seq(0.001,8,0.001)
lines(t,dexp(t,1),col="magenta")
#daca generez un esantion mic, potrivirea nu mai e la fel de spectaculoasa
y2 <-rexp(100,1)
hist(y2,freq=F)
t <- seq(0.001,8,0.001)
lines(t,dexp(t,1),col="magenta")

#4. q+numele repartitiei=functia cuantila(intoarce x-ul corespunzator unei anumite probabilitati)

qnorm(0.95)
qt(0.95,c(1,5,10,15))
qt(0.95, Inf)
qt(0.95,1) #Cauchy
qcauchy(0.95)
qt(c(0.95,0.975,0.99,0.995),c(1,5,10,15))

#Daca folosim functia cuantila pe cazul discret aceasta
#intoarce cel mai mic numar x din supp(X) pentru care
#P(X<=x)>=q
qbinom(c(0.4,0.5,0.6),5,0.5)
#pentru comparatie
pbinom(0:5,5,0.5)



#Repartitia normala->REGINA REPARTIILOR :) 
#Functia densitate de probabilitate a repartitiei normale
t <- seq(-6,6,0.001)
plot(t,dnorm(t,0,1))

plot(t,dexp(t,2),ylim=c(0,0.))
#ATENTIE: IN R parametrii normalei sunt media si abaterea medie standard
y <- rnorm(100,0,1)

poz <- y[y>0]
prob_nr_poz <- length(poz)/10^2
neg <- y[y<0]
prob_nr_neg <- length(neg)/10^2

y <- rnorm(1000000,0,1)

length(y[(y>-3)&(y<3)])

lines(t,dnorm(t,0,1))
plot(t,dnorm(t,3,1),col="magenta",xlim=c(-8,8),ylim=c(0,1))
lines(t,dnorm(t,3,4), col=2)
lines(t,dnorm(t,3,0.5), col=3)
lines(t,dnorm(t,3,2), col=5)
lines(t,dnorm(t,3,0.5),col=1)

z <- rnorm(1000,2,1)
length(z[z< -2])

plot(t,dnorm(t,0,1),col="magenta",ylim=c(0,1.8))
for (i in c(0.25,0.5,0.3,0.9,1.3,2)) lines(t,dnorm(t,0,i), col=i*20)
