library("adabag")
library("MASS")

load("C:/Users/Jeremías/Dropbox/FACU_2017/DM/DM_TP4_Jeremias/TP4.Rdata")

# The class column must be named "class"
ej1a <- function(train,test,rep,treeNumber=200,maxDepth=20){
  
  errors <- matrix(0,maxDepth,rep)
  
  for (depth in 1:maxDepth){
    for (i in 1:rep){
      cat("\nDepth ",depth, "Repetition ",i)
      d.adaboost <- boosting(class ~ ., data = train, mfinal = treeNumber, coef="Freund", control = rpart.control(maxdepth = depth))
      d.pred     <- predict.boosting(d.adaboost, newdata=test)
      errors[depth,i] <- d.pred$error      
      cat("\nError obtained ",d.pred$error)
    }
  }
  
  return(errors)
  
}


espErr <- ej1a(esp_train,esp_test,5)
diagErr <- ej1a(diag_train,diag_test,5)


# The class column must be named "class"
ej1_num <- function(train,test,rep){
  
  errors <- matrix(0,20,rep)
  
  for (nt in 10*(1:20)){
    for (i in 1:rep){
      cat("\nNt ",i, "Repetition ",i)
      esp.adaboost <- boosting(class ~ ., data = train, mfinal = nt, coef="Freund", control = rpart.control(maxdepth = 5))
      esp.pred     <- predict.boosting(esp.adaboost, newdata=test)
      errors[nt/10,i] <- esp.pred$error      
      cat("\nError obtained ",esp.pred$error)
    }
  }
  
  return(errors)
}

errd <- ej1_num(diag_train,diag_test,5)
erre <- ej1_num(esp_train,esp_test,5)



for (nt in c(225, 250, 275, 300, 350)){
  esp.adaboost <- boosting(class ~ ., data = train, mfinal = nt, coef="Freund", control = rpart.control(maxdepth = 5))
  esp.pred     <- predict.boosting(esp.adaboost, newdata=test)
  cat("Error con nt=",nt," error=", esp.pred$error,"\n")
  
}
  
