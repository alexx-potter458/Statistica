#Simularea unei valori dintr-o exp de param lambda

exponentiala <- function(n, lambda) {
  u <- runif(n)
  x <- (-1/lambda) * log(u)
  return(x)
}

val <- 1:5
prob <- c(1/3, 1/30, 2/15, 7/30, 4/15)
u <- runif(1)
if(u < prob[1]) {
  x <- val[1]
}
