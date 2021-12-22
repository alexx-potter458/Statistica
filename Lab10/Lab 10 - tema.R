#Olaru Alexandru Vergiliu - 311



#1)Ilustrati TLC pentru repartitiile: Poisson, Geometrica si Exponentiala pentru toate cele 4 dimensiuni de esantion
autoGraph <- function(X,n,a,b) {
  
  hist(X ,main = c("values: ",n), freq = F)
  abline(v = mean(X))
  t1 <- seq(a, b, 0.001)
  lines(t1, dnorm(t1, mean(X), sd(X)))
  
}

k <- 10000

lambda <- 2

Pois10   <- replicate(k, {x <- mean(rpois(10, lambda))})
Pois100  <- replicate(k, {x <- mean(rpois(100, lambda))})
Pois1000 <- replicate(k, {x <- mean(rpois(1000, lambda))})

par(mfrow = c(1, 3))

autoGraph(Pois10, 10, 0, 4)
autoGraph(Pois100, 100, 0, 4)
autoGraph(Pois1000, 1000, 0, 4)

p <- 0.5

Geom10   <- replicate(k, {x <- mean(rgeom(10, p))})
Geom100  <- replicate(k, {x <- mean(rgeom(100, p))})
Geom1000 <- replicate(k, {x <- mean(rgeom(1000, p))})

par(mfrow = c(1, 3))

autoGraph(Geom10, 10, 0, 4)
autoGraph(Geom100, 100, 0, 4)
autoGraph(Geom1000, 1000, 0, 4)

lambda<-2

Exp10   <- replicate(k, {x <- mean(rexp(10, lambda))})
Exp100  <- replicate(k, {x <- mean(rexp(100, lambda))})
Exp1000 <- replicate(k, {x <- mean(rexp(1000, lambda))})

par(mfrow = c(1, 3))

autoGraph(Exp10, 10, 0, 4)
autoGraph(Exp100, 100, 0, 4)
autoGraph(Exp1000, 1000, 0, 4)



#2)Ilustrati LNM pt. repartiile binomiala, poisson si exponentiala

par(mfrow=c(1,1))

size <- 3
p <- 1/3
lambda <- 2

Binomiala <- function(n) {
  mean(rbinom(n, size, p))
}

Poisson<-function(n) {
  mean(rpois(n, lambda))
}

Exponentiala<-function(n){
  mean(rexp(n, lambda))
}

plot(sapply(1000:10000, Binomiala), type = "o")
abline(h = 1)

plot(sapply(1000:10000, Poisson), type = "o")
abline(h = lambda)

plot(sapply(1000:10000, Exponentiala), type = "o")
abline(h = 1/lambda)


