#Olaru Alexandru / Matei Elena / Negru Bogdan / Trache Andrei 
#1

require("ConvergenceConcepts")

n <- 1000

pnotrgen<-function(n){ rbeta(n, 1/n, 1/n)}
data <- check.convergence(nmax=n, M = 5000, genXn=pnotrgen,mode="p")

pnotrgen<-function(n){ rbinom(n, 1, 1/2)}
data <- check.convergence(nmax=2, M = 5000, genXn=pnotrgen,mode="p")

a <- 100
b <- 100.5

pnotrgen<-function(n){ rbeta(n, a/n, b/n)}
data <- check.convergence(nmax=n, M = 5000, genXn=pnotrgen,mode="p")

pnotrgen<-function(n){ rbinom(n, 1, 1/2)}
data <- check.convergence(nmax=2, M = 5000, genXn=pnotrgen,mode="p")