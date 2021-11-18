#Lab7
#simulam o v.a. Gamma(n, lambda), vezi doc Simulare v.a. continue cu met inversa

#Simulam o val dintr-o Gamma(n, lambda)



gamma_m <- function(nr, n, lambda) {
  X <- c()
  for(i in 1:nr) {
    U <- runif(n)
    X <- c(X, (-2/lamda)* sum(log(U)))
  }
  return(X)
}

n <-  3
lambda <-4
nr <- 10

y <- gamma_m(10^4, n, lambda)

hist(y, freq=F)
t <- seq(0, 3, 0.001)
lines(t, dgamma(t, n, lambda), col="magenta")
