##### EJERCICIO 2

load("lampone.Rdata")
library(MASS)

################# LIMPIEZA DEL DATASET ##############################
#Extraigo la columna con la clase
class <- factor(sapply(lampone[,143],(function (x) if (x==10) return(1) else return(0))))
#Elimino las  columnas no-numéricas, todas cero o con algún otro problema
nums <- sapply(lampone, is.numeric)
lamp <- lampone[ , nums]
i <- (colSums(lamp, na.rm=T) != 0)
lamp <- lamp[, i] 

#Dataset filtrado y con la clase al final
lamp <- cbind(lamp,class)


################### FUNCIONES PARA K-FOLD ###########################

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
  
  clase0 <- dataset[dataset[,d]==0,]
  clase1 <- dataset[dataset[,d]==1,]  
  
  n0 <- dim(clase1)[1]
  n1 <- dim(clase0)[1]  
  
  ind0 <- particion(n0,k)
  ind1 <- particion(n1,k)+n0
  
  # en la fila k está la partición k
  
  return(cbind(ind0,ind1))
}

########################## CALCULAMOS LOS FOLDS ##############################
nfolds <- 5
folds  <- particionar(lamp,nfolds)

############################### RANDOM FOREST ################################

library("randomForest")

lamp.rf <- function() {
  err <- rep(0,nfolds)
  for(i in 1:nfolds){
    train    <- lamp[-folds[i,],]
    test     <- lamp[folds[i,],]
    pred     <- predict(randomForest(class~., train,ntree=1),test)
    hits     <- sum(pred == test$class)
    err[i]   <- 1 - hits/dim(test)[1] 
  }
  return(mean(err))
}

res <- lamp.rf()

############################### BOOSTING ################################
library("adabag")


lamp.boosting <- function(depth){
  err <- rep(0,nfolds)
  for(i in 1:nfolds){
    train    <- lamp[-folds[i,],]
    test     <- lamp[ folds[i,],]
    l.adaboost <-boosting(class ~ ., data = train, mfinal = 100, coef="Freund", control = rpart.control(maxdepth = depth))
    pred       <- predict.boosting(l.adaboost, newdata=test)
    err[i]   <- pred$error
  }
  return(mean(err))  
}

optimParams <- function(maxDepth,rep){
  errd <- matrix(0,maxDepth,rep)
  for (i in 1:maxDepth){
    for (j in 1:rep){
    cat("\nTesting depth ",i, " repetition ",j," \n")
    errd[i,j] <- tryCatch({
      lamp.boosting(i)
    }, error = function(e) {
      0
    })
    cat("Depth ",i," result ", errd[i,j])
    write(c(i,j,errd[i,j]),file="results",append=TRUE)
    }
  }
  return (errd)
}

#p <- optimParams(20,5)
p <- read.table("results",header=FALSE)
p <- matrix(p[,3],nrow = 20, ncol = 5,byrow=TRUE)
sums <- rowSums(p)
okRes <- 5 - rowSums(p==matrix(0,20,5))
errors <- sums/okRes
points <- cbind(1:20,errors)

plot(points,
     main = "Boosting error. Lampone dataset",
     xlab = "Tree depth",
     ylab = "Cross validation error",
     col  = "black",
     lwd = 1,
     ylim = c(0.08,0.2),
     cex = 0.7)
suavizado.datos <- smooth.spline(points,spar=0.45)
lines(suavizado.datos$y,col="red",lwd=1)



############################### SVM ################################

library(kernlab)


svm.gaussian <- function(c,s){
  err <- rep(0,nfolds)
  for(i in 1:nfolds){
    train    <- lamp[-folds[i,],]
    test     <- lamp[ folds[i,],]
    mod      <- ksvm(class~., data=train,kernel="rbfdot",C=c,kpar=list(sigma=s))
    pred     <- predict(mod,test[,-127])
    err[i]   <- sum(pred!=test[,127]) / length(test[,127])
  }
  return(mean(err))  
}


svm.polyn <- function(c,n){
  err <- rep(0,nfolds)
  for(i in 1:nfolds){
    train    <- lamp[-folds[i,],]
    test     <- lamp[ folds[i,],]

    mod      <- ksvm(class~., data=train,kernel="polydot",C=c,kpar=list(degree=n))
    pred     <- predict(mod,test[,-127])
    err[i]   <- sum(pred!=test[,127]) / length(test[,127])
  }
  return(mean(err))  
}


svmG <- function() {
  err <- matrix(0,nrow=21,ncol=19)
  for (c in -5:15){
    for (gamma in -15:3) {
      err[c+6,gamma+16] <- svm.gaussian(2**c,2**gamma)
    }
  }
  return(err)
}

errG <- svmG()

points3 <- cbind(-5:15, errG[,3])
points4 <- cbind(-5:15, errG[,4])
points5 <- cbind(-5:15, errG[,5])
points6 <- cbind(-5:15, errG[,6])
points7 <- cbind(-5:15, errG[,7])

suavizado.datos3 <- smooth.spline(points3,spar=0.45)
suavizado.datos4 <- smooth.spline(points4,spar=0.45)
suavizado.datos5 <- smooth.spline(points5,spar=0.45)
suavizado.datos6 <- smooth.spline(points6,spar=0.45)
suavizado.datos7 <- smooth.spline(points7,spar=0.45)

plot(suavizado.datos4,col="red",lwd=1,type="l",main = "SVM Gaussian kernel. Lampone dataset",
     xlab = "2^c",
     ylab = "Cross validation error",
     cex = 0.7,
     ylim = c(0.15,0.5),
     xlim = c(-5,15))
lines(suavizado.datos5,col="blue")
lines(suavizado.datos6,col="green")
lines(suavizado.datos7,col="pink")
lines(suavizado.datos3,col="black")
legend("topright", title="gamma", legend=c("2^-3", "2^-2", "2^-1","2^0","2^1"),
         col=c("black", "red", "blue","green","pink"), lwd=2)


svmP <- function() {
  err <- matrix(0,nrow=21,ncol=5)
  for (c in -5:15){
    for (d in 1:5) {
      err[c+6,d] <- svm.polyn(2**c,d)
    }
  }
  return(err)
}

errP <- svmP()

points1 <- cbind(-5:15,errP[,1])
points2 <- cbind(-5:15,errP[,2])
points3 <- cbind(-5:15,errP[,3])
points4 <- cbind(-5:15,errP[,4])
points5 <- cbind(-5:15,errP[,5])
suavizado.datos1 <- smooth.spline(points1,spar=0.45)
suavizado.datos2 <- smooth.spline(points2,spar=0.45)
suavizado.datos5 <- smooth.spline(points5,spar=0.45)
suavizado.datos4 <- smooth.spline(points4,spar=0.45)



plot(suavizado.datos4,col="red",lwd=1,type="l",main = "SVM polinomial kernel. Lampone dataset",
     xlab = "2^c",
     ylab = "Cross validation error",
     cex = 0.7,
     xlim = c(-5,15),
     ylim = c(0.07,0.25))
lines(suavizado.datos2,col="green")
lines(suavizado.datos1,col="blue")
lines(suavizado.datos5,col="black")

legend("topright", title="degree", legend=c("1", "2,3", "4","5"),
       col=c("blue", "green", "red","black"), lwd=2)

