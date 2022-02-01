#Olaru Alexandru / Matei Elena / Negru Bogdan / Trache Andrei 
#1

a <- 0
b <- 1
c <- 1
iterations <- 10^7

average <- function() { #calculam numarul de elemente aleatorii necesare din (a,b) pentru a depasi c
  
  if(a+b >= 0) {
    localSum <- 0
    counter <- 0
    
    repeat {
      x <- runif(1, a, b)
      localSum <- localSum + x
      counter <- counter + 1
      if(localSum > c) break  
    
      }
    return(counter)
    
  } else
    return(Inf)
}


aproxK <- sum(replicate(iterations, average()))/iterations #calculam media 


################################################################################
#2

exactK <- ((2*c)/(a+b)) + 1
errorK <- (abs(exactK - aproxK)/exactK)*100 #calculam eroarea in procente