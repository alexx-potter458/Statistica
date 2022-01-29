#Olaru Alexandru / Matei Elena / Negru Bogdan / Trache Andrei 

a <- 0
b <- 1
c <- 1
n <- 10^1

medie <- function() {
  suma <- 0
  counter <- 0
  if(a+b >= 0) {
    while(suma <= c) {
      x <- runif(1, a, b)
      print(suma)
      suma <- suma + x
      counter <- counter + 1
      
    }
  } else
    return(Inf)
  
  return(counter)
}


y <- sum(replicate(n,medie()))/n

max(replicate(n,medie()))

k <- (2*c/(a+b)) + 1

