#Alexandru Olaru 311

#Lista probleme:

#23)
n <- 10^6
u <- runif(n)
X <- log(u*(exp(1)-1)+1)
hist(X,col="blue", freq = F)

#24)
n <- 10^6
u <- runif(n)
ifelse((u>=3/4)&(u<7/2), X <- 1+sqrt(u), ifelse((u>=0)&(u<1/4),X <- 6+2*sqrt(3*u), ifelse(u<0, 0, 1) ))
hist(X,col="blue", freq = F)

#25)

n <- 10^6
u <- runif(n)
ifelse(u<0, X <- 1/2*log(2*u+1), X <- -1/2*log(1-2*u))
hist(X,col="blue", freq = F)

