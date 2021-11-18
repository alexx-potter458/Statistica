#Lab 7 Statistica 311
#Simulam o v.a. Gamma(n,lambda) vezi doc. Simulare v.a. continue cu metoda inversa
#Madalina Badescu
#Simulam o valoare dintr-o Gamma(n,lambda)
n <-3 
lambda <- 4
U <- runif(n)
X <- -1/lambda*sum(log(U))

gamma_madalina <-function(nr,n,lambda) #nr. reprezinta numarul de valori de generat
{
  U <- runif(n)
  X <--1/lambda*sum(log(U))
  for(i in 2:nr)
  {
    U <- runif(n)
    X <-c(X,-1/lambda*sum(log(U)))
    
  }
  return(X)
}
#TEMA: Cum pot evita folosirea for-ului in algoritmul de mai sus
#Evaluati care este imbunatatirea in timpul de executie pe care ati obtinut-o
y <- gamma_madalina(10^4,n,lambda)
hist(y,freq=F)
t <- seq(0,3,0.001)
lines(t,dgamma(t,n,lambda),col="magenta")
