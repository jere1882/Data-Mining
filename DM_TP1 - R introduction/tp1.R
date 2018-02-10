#################### EJERCICIO 1  ####################

###### APARTADO A ######

# Esta funcion genera un dataset del problema Diagonal
genera_diagonal <- function(C,d,n) {
  
  ds <- C*sqrt(d)     # Desviacion estandar
  n0 <- ceiling(n/2)  # Numero de puntos de clase 0
  n1 <- n/2           # Numero de puntos de clase 1

  puntos0 <-  matrix( replicate(n0, rnorm(d,-1,ds)) , ncol=d , byrow = T)  # puntos de clase 0  
  puntos1 <-  matrix( replicate(n1, rnorm(d, 1,ds)) , ncol=d , byrow = T)  # puntos de clase 1

  filas0  <-  cbind (puntos0, rep(0,n0))   # filas finales del dataset
  filas1  <-  cbind (puntos1, rep(1,n1))   # filas finales del dataset

  dataset <- rbind(filas0,filas1)   
  colnames(dataset)[d+1] <- "c"   # Renombro esta columna para usar rpart
  return(data.frame(dataset))
}


###### APARTADO B ######

# Funciones auxiliares

# Radio de un punto
radio <- function(x,y) {
  return (sqrt(x**2 + y**2))
}
# Ángulo (en coord polares)
angle <- function(x,y) {
  return (atan2(y,x))
}
# Bordes de las espirales
ro1 <- function (theta) {
  return (theta/(4*pi))
}
ro2 <- function (theta){
  return ((theta+pi)/(4*pi))
}
# Genera un punto random dentro de la circunferencia
genera_circ <- function(){
  x <- runif(1,-1,1)
  y <- runif(1,-1,1)   	
  if ( radio(x,y) <=1 ){ 
    return (c(x,y))
  }
  else{
    return(genera_circ())
  }
}
# Clasifica ese punto de la circunferencia en clase 0 ó 1
classify <- function(x,y){
  
  r <- radio(x,y)
  theta <- angle(x,y)
  
  # Para clasificar, lo que hago es hallar los 2 puntos de las curvas que se intersectan con la
  # semirrecta que pasa por (0,0) y por (x,y).
  # Hallados estos 4 puntos, es facil clasificar a (x,y) viendo entre cuales está su módulo  	
  # Empíricamente encontré que a lo sumo en 4 vueltas salen los radios que preciso.....
  
  c1 <- sapply(theta + 2*(-2:2)*pi , ro1)
  c2 <- sapply(theta + 2*(-2:2)*pi , ro2)
  
  
  
  # Ordeno de menor a mayor los radios 
  
  s1 <- sort(c1)
  s2 <- sort(c2)
  
  i1 <- s1[s1 >= 0]  # u1 u2
  i2 <- s2[s2 >= 0]  # v1 v2
  
  # Estas son las 4 intersecciones de la semirecta que sale del orígen
  # y pasa por el punto (x,y) ; con las curvas ro1 y ro2
  
  #Analizo entre cuales de las intersecciones está
  
  if (y<0){
    if ( r > i1[2] || r < i2[1] || (r > i1[1] && r < i2[2]))
      return(1)
    return(0)
  }
  else {
    if ( r > i2[2] || r < i1[1] || (r > i2[1] && r < i1[2] ))
      return(0)
    return(1)
  }	
  
}
# Genera un punto random de clase 1
genera_c1 <- function(){
	p <- genera_circ()
	if (classify(p[1],p[2])==1) {
		return(p)
	}
	else {
		return(genera_c1())
	}
}
# Genera un punto random de clase 0
genera_c0 <- function(){
	p <- genera_circ()
	if (classify(p[1],p[2])==0) {
		return(p)
	}
	else {
		return(genera_c0())
	}
}

# Genera un dataset con n puntos del problema Espirales Anidadas
genera_espirales <- function(n) {

  n0 <- ceiling(n/2)  # Numero de puntos de clase 0  
  n1 <- n/2           # Numero de puntos de clase 1

  puntos0 <-  matrix( replicate(n0, genera_c0()) , ncol=2 ,byrow =T)  # puntos de clase 0  
  puntos1 <-  matrix( replicate(n1, genera_c1()) , ncol=2 ,byrow=T )  # puntos de clase 1

  filas0  <-  cbind (puntos0, rep(0,n0))   # filas finales del dataset
  filas1  <-  cbind (puntos1, rep(1,n1))   # filas finales del dataset

  dataset <- rbind(filas0,filas1)   
  colnames(dataset)[3] <- "c" # para poder usar DTL facil  
  return(data.frame(dataset))
}	


#################### EJERCICIO 2  ####################


# Genero los datasets de ejemplo

espiral_ejemplo   <- genera_espirales(2000)
diagonal_ejemplo1 <- genera_diagonal(0.2,2,1000)
diagonal_ejemplo2 <- genera_diagonal(0.5,2,2000)


# Ploteo espiral
pdf("espirales.pdf")
plot(espiral_ejemplo[,1], espiral_ejemplo[,2],col=espiral_ejemplo[,3]+3, main = "Espirales anidadas, ejemplo"   , xlab = "x"
     , ylab = "y")
abline(h=0,col="black")
abline(v=0,col="black")
dev.off()

# Ploteo diagonal - alejadas
pdf("diagonal1.pdf")
plot(diagonal_ejemplo1[,1], diagonal_ejemplo1[,2],col=diagonal_ejemplo1[,3]+3, main = "Dataset diagonal, ejemplo 1"   , xlab = "x"
     , ylab = "y")
abline(h=0,col="black")
abline(v=0,col="black")
dev.off()

# Ploteo diagonal - superpuestas
pdf("diagonal2.pdf")
plot(diagonal_ejemplo2[,1], diagonal_ejemplo2[,2],col=diagonal_ejemplo2[,3]+3, main = "Dataset diagonal, ejemplo 2"   , xlab = "x"
     , ylab = "y")
abline(h=0,col="black")
abline(v=0,col="black")
dev.off()


#################### EJERCICIO 3  ####################

# Generamos los datasets de test y training

e.train <- genera_espirales(200)
e.test  <- genera_espirales(2000)

d.train <- genera_diagonal(0.8,2,200)
d.test  <- genera_diagonal(0.8,2,2000)


##### PARTE 1 - USANDO TODO EL CONJUNTO DE TRAINING #####

# Ajustando KNN

library(class)

# estas funciones serán mapeada a distintos valores de k
# para hallar el valor optimo de k-nn

knnfc_e <- function(n) {
  prediccion <- knn(e.train[,1:2],e.test[,1:2],e.train[,3],k=n) 
  aciertos   <- sum(prediccion == e.test[,3])
  return(aciertos)
}
  
knnfc_d <- function(n) {
  prediccion <- knn(d.train[,1:2],d.test[,1:2],d.train[,3],k=n) 
  aciertos   <- sum(prediccion  == d.test[,3])
  return(aciertos)
}


# Aplicamos KNN con varios k
resultados_e <- sapply(1:20,knnfc_e)
resultados_d <- sapply(1:100,knnfc_d)

# Hallamos los k optimos
bestk_e <- which.max(resultados_e)
bestk_d <- which.max(resultados_d)

# Calculamos el error 
knn_errt_e <- 1- resultados_e[bestk_e]/dim(e.test)[1]
knn_errt_d <- 1- resultados_d[bestk_d]/dim(d.test)[1]

cat("\nResultados usando el conjunto entero para training:\n")

cat("\nKNN, Espirales anidadas. Mejor k:", bestk_e)
cat("  Error en test: ", knn_errt_e)

cat("\nKNN, Problema diagonal.  Mejor k:", bestk_d)
cat(" Error en test: ", knn_errt_d)


# AJUSTANDO DTL
library(rpart)

diagonal.dtl  <- rpart("c~ .",data=d.train,method="class")
diagonal.pred <- matrix( predict(diagonal.dtl,d.test[,1:2],type="class") )

espirales.dtl  <- rpart("c~ .",data=e.train,method="class")
espirales.pred <- matrix( predict(espirales.dtl,e.test[,1:2],type="class") )

aciertos.d  <- sum( diagonal.pred  == d.test[,3])
aciertos.e  <- sum(espirales.pred  == e.test[,3])

# Calculamos el error 
dtl_errt_e <- 1- aciertos.e /dim(e.test)[1]
dtl_errt_d <- 1- aciertos.d /dim(d.test)[1]

cat("\nDTL, Espirales anidadas. Error en test: ", dtl_errt_e)
cat("\nDTL, Problema diagonal.  Error en test: ", dtl_errt_d)


##### PARTE 2 - USANDO 5-FOLD CROSS VALIDATION #####

# Particiona el conjunto {1,size} en k conjuntos aleatorios del mismo tamaño
# Si size no es divisible por k, se tiran size%%k elementos aleatorios
particion <- function(size,k){
  ind  <- sample(1:size)
  step <- floor(size/k)
  ans <- Map( function(i)  ind[ ((i*step)+1):(i*step + step)] , 0:(k-1))
  return(do.call(rbind, ans))
}


# Funcion util para particionar un dataset y luego realizar cross validation
# solo funciona para datasets con dos clases, 0 y 1
# devuelve una matriz de k filas donde cada fila son los indices de una partición
particionar <- function(dataset,k) {
  n <- dim(dataset)[1]
  d <- dim(dataset)[2]
  
  clase0 <- dataset[dataset[,3]==0,]
  clase1 <- dataset[dataset[,3]==1,]  

  n0 <- dim(clase1)[1]
  n1 <- dim(clase0)[1]  
  
  ind0 <- particion(n0,k)
  ind1 <- particion(n1,k)+n0
  
  # en la fila k está la partición k
  
  return(cbind(ind0,ind1))
}


# funcion que se usara en un map para aplicar knn a uno de los folds
knn_evd <- function(n) {
  train <- d.train[-folds.d[n,],]
  test  <- d.train[folds.d[n,],]
  bestk_d = min(bestk_d,160)
  prediccion <- knn(train[,1:2],test[,1:2],train[,3],k=bestk_d) 
  aciertos   <- sum(prediccion  == test[,3])
  return(aciertos)
}

folds.d       <- particionar(d.train,5)    # particionamos el conjunto de training en 5
res_fold.d    <- sapply(1:5,knn_evd)       # entrenamos 5 veces con cada fold
errores.d     <- res_fold.d/40             # calculamos los aciertos
avgerr.d      <- 1- mean(errores.d)        # calculamos el promedio de errores

# funcion que se usara en un map para aplicar knn a uno de los folds
knn_eve <- function(n) {
  test       <- e.train[folds.e[n,],]
  train      <- e.train[-folds.e[n,],]
  bestk_e    <- min(bestk_e,160)
  prediccion <- knn(train[,1:2],test[,1:2],train[,3],k=bestk_e) 
  aciertos   <- sum(prediccion  == test[,3])  
}

folds.e      <- particionar(e.train,5)
res_fold.e   <- sapply(1:5,knn_eve)     # iteramos eligiendo cada fold como training
errores.e     <- res_fold.e/40
avgerr.e      <- 1 - mean(errores.e)

cat("\n")
cat("\nResultados usando estimacion en 5-fold:\n")

cat("\nKNN, Espirales anidadas. Error en test: ", avgerr.e)
cat("\nKNN, Problema diagonal.  Error en test: ", avgerr.d)

dtl_eve <- function(n) {
  train <- e.train[-folds.e[n,],]
  test  <- e.train[folds.e[n,],]
  espirales.dtl  <- rpart("c~ .",data=train,method="class")  
  espirales.pred <- matrix( predict(espirales.dtl,test[,1:2],type="class") )
  aciertos.e   <- sum( espirales.pred  == test[,3])  
  return(1- aciertos.e /dim(test)[1])
}


dtl_evd <- function(n) {
  train <- d.train[-folds.d[n,],]
  test  <- d.train[folds.d[n,],]
  diagonal.dtl  <- rpart("c~ .",data=train,method="class")  
  diagonal.pred <- matrix( predict(diagonal.dtl,test[,1:2],type="class") )
  aciertos.d   <- sum( diagonal.pred  == test[,3])  
  return(1- aciertos.d /dim(test)[1]) 
}


res_fold.t.e    <- sapply(1:5,dtl_eve)     # iteramos eligiendo cada fold como training
avgerr.t.e      <- mean(res_fold.t.e)

res_fold.t.d    <- sapply(1:5,dtl_evd)     # iteramos eligiendo cada fold como training
avgerr.t.d      <- mean(res_fold.t.d)

cat("\nDTL, Espirales anidadas. Error en test: ", avgerr.t.e)
cat("\nDTL, Problema diagonal.  Error en test: ", avgerr.t.d)
cat("\n")

