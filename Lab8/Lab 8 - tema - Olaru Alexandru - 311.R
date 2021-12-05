#311 - Olaru Alexandru

#1) De implementat algoritmul de la metoda respingerii din exemplul scris(vezi pdf)

MetRespingerii <- function() {
  iMR <<- iMR + 1
  
  Y <- runif(1, min = 0, max = 1)
  U <- runif(1, min = 0, max = 1)
  
  ifelse(U <= ((256/27) * Y * ((1 - Y)**3 )), return(Y), MetRespingerii())
}

iMR <- 0
Beta <- MetRespingerii()
cat("X=", Beta, ", counter= ", iMR)


#2) De implementat algoritmul de la metoda respingerii pt repartitia normala standard(vezi pdf)

MetRespingeriiNormala <- function() {
  iMRN <<- iMRN + 1
  
  Y <- rexp(1)
  U1 <- runif(1, min = 0, max = 1)
  U2 <- runif(1, min = 0, max = 1)
  
  ifelse(U1 <= exp(-(Y-1)^2/2), ifelse(U2 <= 0.5, return(-Y), return(Y)), return(MetRespingeriiNormala()))
}

iMRN <- 0
x <- MetRespingeriiNormala()
cat("X=", x, ", counter= ", iMRN)



#Lista probleme:

#23) f(x) = e^x/(e-1), 0<=x<=1

MetRespingerii <- function() {
  iMR <<- iMR + 1
  
  Y <- runif(1, min = 0, max = 1)
  U <- runif(1, min = 1/exp(1), max = 1)
  
  ifelse(U <= (exp(Y-1)), return(Y), MetRespingerii())
}

iMR <- 0
X <- MetRespingerii()
cat("X=", X, ", counter= ", iMR)


#24) f(x) = { (x-2)/2 , 2 <=x<= 3
#           { (2 - (x/3))/2, 3 <= x <= 6

MetRespingerii <- function() {
  iMR <<- iMR + 1
  
  Y <- runif(1, min = 2, max = 6)
  U <- runif(1, min = 0, max = 1)
  
  ifelse((U <= (Y-2) & Y >= 2 & Y <= 3) | (U <= (2 - (Y/3)) & Y >= 3 & Y <= 6), return(Y), MetRespingerii())
}

iMR <- 0
X <- MetRespingerii()
cat("X=", X, ", counter= ", iMR)



