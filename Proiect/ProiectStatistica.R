#Olaru Alexandru / Matei Elena / Negru Bogdan / Trache Andrei

# I.)
## 1.

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
## 2.

exactK <- ((2*c)/(a+b)) + 1
errorK <- (abs(exactK - aproxK)/exactK)*100 #calculam eroarea in procente

# II.)
## 1.

iterations <- 10^7 #numarul de persoane
n <- 20 #numarul de etape

alfa <- runif(n-1, 0, 1) #probabilitatea ca o persoana sa termine fiecare activitate, ca vector
Ti <- rexp(n, runif(1, 0, 1)) #timpul necesar pentru fiecare activitate, ca vector
  
runStages <- function() { #persoana isi parcurge etapele pana la esec
  totalTime <- Ti[1]
  nextStage <- runif(1, 0, 1)
  
  if(nextStage < alfa[1]) {
    totalTime <- totalTime + Ti[2]
  
      for (i in 2:(n-1)) {
        nextStage <- runif(1)
    
        if(nextStage < alfa[i]) {
          totalTime <- totalTime + Ti[i+1]
      }
    }
  }
  
  return(totalTime)
}

totalTimes <- replicate(iterations,runStages()) #timpul T pentru fiecare persoana, ca vector
averageTime <- sum(totalTimes)/iterations # media timpului necesar fiecarei persoane

################################################################################
## 3.

finishProb <- prod(alfa) #se inmultesc probabilitatile din alfa

################################################################################
## 4.

sigma <- runif(1, min(Ti), max(Ti)) #generam un timp aleator
counter <- 0
timeSum <- 0

if(sigma >= Ti[1]) { #daca sigma e prea mic atunci nu se termian prima etapa
  
  for(i in 1:n) {
    
    if((timeSum + Ti[i]) <= sigma) {
      timeSum <- timeSum + Ti[i]
      counter <-  counter + 1
      
    } else {
      break
    }
  }
  sigmaProb <- prod(alfa[1:(counter-1)])*(1-alfa[counter])
  
} else {
  sigmaProb <- 0
  
}

################################################################################
## 5.

minTime <- min(totalTimes) #timpul minim
maxTime <- max(totalTimes) #timpul maxim
hist(totalTimes)

################################################################################
## 6.

k <- 5

probabilities <- c(1 - alfa[1])
time <- Ti[1]

for (i in 2:(n-1)) {
  tempValue <- 1
  
  for(j in 1:(i-1)) {
    tempValue <- tempValue * alfa[j]
    
  }
  
  tempValue <- tempValue * (1 - alfa[i])
  probabilities <- c(probabilities, tempValue)
  
}

kProbabilities <- cumprod(probabilities) #vectorul de probabilitati de a ajunge la k
kProb <- kProbabilities[k] #prbabilitatea de ajunge la etapa k

plot(kProbabilities)
lines(kProbabilities)

# III.)

require("ConvergenceConcepts")

## 1.

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


## 2.
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


## 3.



# IV.)

