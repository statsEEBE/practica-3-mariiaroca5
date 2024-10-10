#ensayo de bernoulli

x <- c(0,1)
f <- c(0.68, 0.32)

plot(x, f, type="h", ylim=c(0,1), col="red")
points(x, f, pch=16, col="red")#escribimos puntos encima


n <- 43
muestra <- sample(x, n, f, replace=TRUE) #la prob es f, replace(volvemos a poner)

pie(table(muestra))
table(muestra)/n #frecuencia relativa (/n)
mean(muestra)

bar <- barplot(table(muestra)/n, ylim=c(0,1) #son como las chinchetar
lines(bar, f, type="h", col="red")
points(bar, f, pch=16, col="red") #nose pq no me va, son ses columnes amb xinxetes


#seguim amb s'exercici
muestra <- sample(x, n, f, replace=TRUE)
muestra

Y <- function(i){sum(sample(x, n, f, replace=TRUE))}
Y(1)
Y(2) 
#ES UNA ENCUESTA, VAS SACANDO NUM Y VEMOS LA FREC RELATIVA (la remetimos 40)

m <- 40
hist(sapply(1:m, Y))


set.seed(123) #si ponemos esto a todos nos sale el mismo valor, sol numerica al problema
m <- 400000
encuestas <- sapply(1:m, Y) #ahora tendriamos que ir a 13 (como dice el ejercicio) y coger el numero

fr <- table(encuestas)/m #fr = frecuencias relativas
fr["13"] #nos sale el num: 0.128775

#el problema se puede resolver con una linea de codigo
dbinom(13, 43, 0.32)

xx <- names(fr)
xx

br <- barplot(table(encuestas)/m)
lines(br, dbinom(2:29, 43, 0.32), type="h", col= "red") #???



#segon apartat ex 1
dbinom(17, 44, 0.32) #posibilidad de que salga 17
plot(0:43, dbinom(0:43, 44, 0.32), type="h",  col= "red")

pbinom(16, 44, 0.32)        #posibilidad de que salga menos de17



####
n <- 2
x <- c(0,1)
f <- c(0.32, 0.68)

Xstar <- function(i){sum(sample(x, n, f, replace=TRUE))}

set.seed(123)
m <- 400000
encuestas <- sapply(1:m, Xstar)
mean(encuestas)
n*0.68
var(encuestas)

qbinom(0.25,)
plot(0:24, dbinom(0))
46*0.32


#median es acomulo los datos y corto por la mitad
#mean es el centro de gravedad, la suma de los datos 
