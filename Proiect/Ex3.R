#Olaru Alexandru / Matei Elena / Negru Bogdan / Trache Andrei 

require("ConvergenceConcepts")

#1

n <- 1000

valuesGenBeta <- function(n) {rbeta(n, 1/n, 1/n)}
valuesGenBin  <- function(n) {rbinom(n, 1, 1/2)}

dataBetaL <- check.convergence(nmax = n, M = 5000, genXn = valuesGenBeta, mode = "L")
dataBinL  <- check.convergence(nmax = 2, M = 5000, genXn = valuesGenBin,  mode = "L")

a <- runif(1, 0, 100)
b <- runif(1, 0, 100)

valuesGenBetaC <- function(n) {rbeta(n, a/n, b/n)}

dataBetaLC <- check.convergence(nmax = n, M = 5000, genXn = valuesGenBetaC, mode = "L")
dataBinL   <- check.convergence(nmax = 2, M = 5000, genXn = valuesGenBin,   mode = "L")


#2
valuesGenUnif <- function(n) {runif(n, 0, 1)}
valuesGenUnifC <- function(n) { 
  X <- c()
  
  for(i in 1:n) {
    X  <- c(X,(runif(1, i/n, i/n)))
  }
  return(X)
}

dataUnifLC <- check.convergence(nmax=n, M = 5000, genXn=valuesGenUnifC, mode="L")
dataUnifL  <- check.convergence(nmax=2, M = 5000, genXn=valuesGenUnif,  mode="L")

dataUnifPC <- check.convergence(nmax=n, M = 5000, genXn=valuesGenUnifC, mode="p")
dataUniPL  <- check.convergence(nmax=2, M = 5000, genXn=valuesGenUnif,  mode="p")


#3



