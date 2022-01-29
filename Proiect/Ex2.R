#1

iterations <- 10^6
n <- 10
alfa <- runif(n-1, 0, 1)

etapa <- function(){
  u <- rexp(1, runif(1, 0, 1))
}

ti <- replicate(n, etapa())

parcurgere <- function() {
  time <- ti[1]
  u2 <- runif(1, 0, 1)
  if(u2 < alfa[1]) {
    time <- time + ti[2]
    for (i in 2:(n-1)) {
      u3 <- runif(1)
      if(u3 < alfa[i]) {
        time <- time + ti[i+1]
      }
    }
  }
  
  return(time)
}

times <- replicate(iterations,parcurgere())

ET <- sum(times)/iterations

hist(times, freq = F)


#3
prob <- prod(alfa)

#4
timpi <- c(ti[1])
probabilitati <- c(1 - alfa[1])
time <- ti[1]
for (i in 2:(n)) {
  time <- time + ti[i]
  timpi <- c(timpi, time)
  temporaryValue <- 1
  for(j in 1:(i-1)) {
    temporaryValue <- temporaryValue * alfa[j]
  }
  temporaryValue <- temporaryValue * (1 - alfa[i])
  
  probabilitati <- c(probabilitati, temporaryValue)
}
probabilitati <- c(probabilitati, prod(alfa)

ExpectedValue = sum(probabilitati * timpi)

