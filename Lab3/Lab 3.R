x <- 3.14
y <- 2.44
z <- x + y

typeof(z)

nr_studenti <- as.integer(28)

typeof(nr_studenti)

complex1 <- as.complex(-2+2i)
complex2 <-  complex(3, 10, 2)
complex3 <- rep(as.complex(10+2i), 3)

vector_numeric <-  c(1, 2, 10)
class(vector_nuumeric)

vector_caracter <- c('Text', 'nou')
class(vector_caracter)

(vector_integer <- 1:10)
class(vector_integer)

class(vector_logic <- c(T, F))
vector_mixt <- c(1,2, "s", T)
class(vector_mixt)

x <- sample(1:100, 10)
y <- (x[1:50]<30)

is.na(y)

z <- x[1:5]<x[6:10]
y <- y[!is.na(y)]
y1 <- c(NA, NA, NA)
y1 <- y1[which(!is.na(y1))]

x <-  sample(1:100, 10)t[which(isTRUE(T))]

t <- (x[2* 1:5-1] < x[2* 1:5])

t[which(t==T)]

x <- 483:5879

length(x[(x%%8==0)&(x!=0)])
