#Olaru Alexandru / Matei Elena / Negru Bogdan / Trache Andrei 
#1

a <- 0
b <- 1
c <- 1
iterations <- 10^9

average <- function() { #calculam numarul de elemente aleatorii necesare din (a,b) pentru a depasi c
  suma <- 0
  counter <- 0
  if(a+b >= 0) {
    while(suma <= c) {
      x <- runif(1, a, b)
      suma <- suma + x
      counter <- counter + 1
      
    }
  } else
    return(Inf)
  
  return(counter)
}


aproxK <- sum(replicate(iterations, average()))/iterations #calculam media 


################################################################################
#2

a <- 0
b <- 1
c <- 1
exactK <- (2*c/(a+b)) + 1


errorK <- (abs(exactK - aproxK)/exactK)*100