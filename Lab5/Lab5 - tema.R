#1) Creati o functie in R numita gama_nume care sa implementeze proprietatile
#pe care le are functia gama(vezi documentul Integrale euleriene) si sa 
#foloseasca apelul functiei integrate doar atunci cand parametrul nu satisface 
#nicio conditie "buna"

# gama_nume <- function(....)
#{
#daca n e natural atunci foloseste propr.3)     #folosim functia din R numita factorial
#daca n e de forma b/2(cu b natural) foloseste formula 2) si 4)
#altfel foloseste formula 2) pana cand argumentul devine subunitar
#si doar pentru acea valoare calculeaza cu integrate

gamma_alex <- function(n) {

  if(n == 1)
    return(1)
  
  if(n == 1/2)
    return(pi^(1/2))
  
  result <- 0
  
  if( n > 1) {
    if(n == round(n)) {
      result <- factorial(n-1)
      return(result)
    } else {
      result <- (n-1) * gamma_alex(n-1)
      return(result)
    }
  } else {
    f <- function(x,n) {
      x^(n-1)*exp(-x)
    }
    result <- integrate(f,0,Inf,n)
    return(result$value)
  }
}


gamma_alex(1)
gamma_alex(1/3)
gamma_alex(5/2)
gamma_alex(11)
#2) Implementati o functie care calculeaza beta(a,b) folosindu-va de proprietati
#si de integrala gama

beta_alex <- function(a, b) {
  if(a>0 & b>0) {
    if(a+b == 1) {
      result <- pi/sin(a*pi)
      return(result)
    }
    
    result <- (gamma_alex(a)*gamma_alex(b))/gamma_alex(a+b)
    return(result)
  }
}

beta_alex(1,2)
