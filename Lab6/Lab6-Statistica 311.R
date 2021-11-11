#Simularea unei valori dintr-o exponentiala de parametru lambda
#Marius Radu
lambda <- 4
u <- runif(1)
x <- -1/lambda*log(u)

#Scrieti o functie ce genereaza n valori din Exp(lambda)
expo_marius <-function(n,lambda)
{
  u <- runif(n)
  x <- -1/lambda*log(u)
  return(x)
}
#Vizualizam valorile generate prin intermediul unei histograme
lambda <- 0.3
n <- 10^6
y <- expo_marius(n,lambda)
#Cate valori am cuprinse intre 0 si 0.2
length(y[(y<=0.2)&(y>=0)])
max(y)
hist(y,col="magenta",freq=F)
t <- seq(min(y),max(y),0.001)
lines(t,dexp(t,lambda),col="blue")
y1 <- rexp(n,lambda)
#hist(y1)
hist(y1,add=T,col="blue",freq=F)

#TEMA: Construiti un algoritm care simuleaza 10^6 valori din repartitia
#lucrata in exemplul cu metoda inversa

# Algoritmul pentru simularea unei valori dintr-o variabila aleatoare
#discreta X:(1 2 3 4 5;10/30 1/30 4/30 7/30 8/30)
val <- 1:5
prob <- c(1/3,1/30,2/15,7/30,4/15)
u <- runif(1)
if (u<prob[1]) { x=val[1]
} else if (u<prob[1]+prob[2]) {x=val[2]
}else {if (u<prob[1]+prob[2]+prob[3]) x=val[3]
else  if (u<prob[1]+prob[2]+prob[3]+prob[4]) x=val[4]
else x=val[5]
}  

prob_partiale <- cumsum(prob)

#TEMA: Eficientizati cat de mult posibil algoritmul de mai sus