# Algoritmul pentru simularea unei valori dintr-o variabila aleatoare
#discreta X:(1 2 3 4 5;10/30 1/30 4/30 7/30 8/30)
#val <- 1:5
#prob <- c(1/3,1/30,2/15,7/30,4/15)
#u <- runif(1)
#if (u<prob[1]) { x=val[1]
#} else if (u<prob[1]+prob[2]) {x=val[2]
#}else {if (u<prob[1]+prob[2]+prob[3]) x=val[3]
#else  if (u<prob[1]+prob[2]+prob[3]+prob[4]) x=val[4]
#else x=val[5]
#}  
#TEMA: Eficientizati cat de mult posibil algoritmul de mai sus

val <- 1:5
prob <- c(1/3,1/30,2/15,7/30,4/15)
u <- runif(1)
for(i in val) {
  if(u < cumsum(prob[1:i])[i]) {
    x <- val[i]
    break
  }
}

prob_partiale <- cumsum(prob)





##Lista probleme:

#1 F(x) = x^n = u => x = u^(1/n)
n = 10^6
u <- runif(n)
X <- u^(1/n)
hist(X,col="magenta",freq=F)

#2 F(x) = x(x+1)/2 = u => x = (-1+-sqrt(1+4u))/2
n = 10^6
u <- runif(n)
X_poz <- (-1+(1+4*u)^(1/2))/2
X_neg <- (-1-(1+4*u)^(1/2))/2
hist(X_poz,col="blue", freq = F)
hist(X_neg, col="gray", freq = F)

#3 
n <- 10^6
alfa <- 2
beta <- 2
u <- runif(n)
X <- (log(1/(1-u))/alfa)^(1/beta)
hist(X,col="blue", freq = F)

#4
n <- 10^4
u <- runif(n, 0, 4)
rightValue <- function(x) {
  if(x < 0) {
    return(0)
  } else if(x >= 0 & x < 1) {
    return(x/2)
  } else if(x >= 1 & x < 2) {
    return(1/2)
  } else if(x >= 2 & x < 4) {
    return(x/4)
  } else {
    return(1)   
  }
}

X <- c()
for(i in u) {
   X <- c(X, rightValue(i))
}
hist(X,col="blue", freq = F)

#5 
n <- 10^6
b <- 4
u <- runif(n, 0, 1)
X <- ((-b)*(log(1-u)))^(1/2)
hist(X,col="blue", freq = F)

#6
n <- 10^6
u <-  runif(n)
X <- tan((u-(1/2)*pi))
hist(X,col="blue", freq = F)

#7
n <- 10
u <- runif(n)
X <- log((tan((pi/2)*u)))
hist(X,col="blue", freq = F)

#10
n <- 10^2
u <- runif(n, 0, 1)
rightValue <- function(x) {
  if(x < 1/exp(1)) {
    return(0)
  } else if(x >= 1/exp(1) & x < exp(1)) {
    return((1 + log(x))/2)
  } else {
    return(1)   
  }
}

X <- c()
for(i in u) {
  X <- c(X, rightValue(i))
}
hist(X,col="blue", freq = F)

X#11
n <- 10^6
u <- runif(n)
X <- 2*(u^2) + 1
hist(X,col="blue", freq = F)