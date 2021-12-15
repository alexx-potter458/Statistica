#Olaru Alexandru - 311

#1) Rescrieti functia f_respingere_n astfel incat sa nu se mai bazeze pe apelul 
#functiei f_respingere

f_respingere_n <- function(n)
{ 
  xk <- replicate(n, {
    ok <- F
    k <- 0
    while(ok==F) {
      y <- runif(1,0.8,1)
      u <- runif(1)
      if(u<=625/4*y*(1-y)^3) {
        x <- y
        ok <- T
      }
      k <- k+1
    }
    
    return(c(x,k))
  })
  
  return(xk)
}

a <- f_respingere_n(10^6)

#2): Rescrieti functia fbeta_n astfel incat sa genereze n valori
#folosind un algoritm care nu mai apeleaza functia fbeta

fbeta_n <- function(n)
{
  xk <- replicate(n, {
    ok <- F
    k <- 0
    while(ok==F)
    {
      u <- runif(2)
      if(u[2]<=256/27*u[1]*(1-u[1])^3) {
        x <- u[1]
        ok <- T
      }
      k <- k+1
    }  
    return(c(x,k)) 
  })
  
  return(xk)
}


b <- fbeta_n(10^6)

#3): FOlositi metoda respingerii pentru a genera n valori dintr-o v.a. discreta
# data de valorile -5:4 cu probabilitatile (1/50,2/50,10/50,11/50,9/50,1/50,1/50,3/50,3/50,3/50)

x <- c(-5:4)
p <- c(1/50,2/50,10/50,11/50,9/50,1/50,1/50,3/50,3/50,3/50)
a <- cbind(x,p)
n <- 10


discretan <- function(n){
  
  discreta <-  function(foobar){
    X <- a[1,1]
    u <- runif(1)
    for( i in 1:n){
      X <- ifelse(sum(a[i,2])<=u & u<sum(a[i,2]), X <- a[i,1], X)
      break;
    }
    return(X);
  }
  sol <- lapply(1:n,discreta)
  print(sol)
  return(sol);
}

discretan(n)