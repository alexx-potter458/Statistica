#Olaru Alexandru / Matei Elena / Negru Bogdan / Trache Andrei 

#1

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

#3

finishProb <- prod(alfa) #se inmultesc probabilitatile din alfa


################################################################################

#4

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

#5

minTime <- min(totalTimes) #timpul minim
maxTime <- max(totalTimes) #timpul maxim
hist(totalTimes)


################################################################################

#6

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