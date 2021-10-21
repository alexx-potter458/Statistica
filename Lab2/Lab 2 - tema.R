#1) Creati o variabila aleatoare discreta X care ia valorile 1 2 si respectiv 3
#cu probabilitatile 1/2, 1/3 si respectiv 1/6 si calculati probabilitatile
#a) P(X<2.7)
#b) P(0.5<X<3)
#c) P(X>2|X>1)
# Generalizati solutia pentru o v.a. ce ia valori de la 1 la n(cu n fixat)
#iar probabilitatile sunt generate aleator folosind functia sample()

library(prob)

x <- c(1,2,3)
p <- c(1/2,1/3,1/6)

pb <- probspace(x,p)
#a)
Prob(pb,x<2.7)
#b)
Prob(pb,x<3 & x>0.5)
#c)
Prob(pb,x>2, x>1)

#Generalizare pentru n fixat
n <- 100

vectorValori <- c(1:n)
probabilitati <- sample(seq(0,1,0.002),n)
probabilitati/sum(probabilitati)
pb2 <- probspace(vectorValori,probabilitati)

Prob(pb2,x<2.7)
Prob(pb2,x<3 & x>0.5)
Prob(pb2,x>2,x>1 )


#2) Construiti doi vectori x si y cu 1000 de elemente fiecare, extrase in mod
#aleator din multimea cu numere intregi -34000:45000. Stabiliti care dintre cei
#doi vectori are mai multe elemente, luate in valoare absoluta, mai mari decat
#valoarea absoluta a elementului corespondent din celalalt vector

x <- sample(seq(-34000, 45000, 1))
y <- sample(seq(-34000, 45000, 1))

values_x <- 0
values_y <- 0

for(i in 1:length(x)) 
  if(abs(x[i] > abs(y[i]))) 
    values_x <- values_x +1

for(i in 1:length(x)) 
  if(abs(x[i] < abs(y[i]))) 
    values_y <- values_y +1

  
if(values_x > values_y)
  print("Vectorul x e mai mare")
if(values_x < values_y)
  print("Vectorul y e mai mare")



help("sample")

