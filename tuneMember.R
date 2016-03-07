tuneMember <- function(call,tuning,xtest,ytest, predicttype=NULL,probability=TRUE){
  if (require(AUC)==FALSE) install.packages("AUC"); library(AUC)
  
  grid <- expand.grid(tuning)
  
  perf <- numeric()
  for (i in 1:nrow(grid)){
  Call <- c(as.list(call), grid[i,])
  model <-  eval(as.call(Call))
 
  predictions <- predict(model,xtest,type=predicttype, probability=probability)    
  
  if (class(model)[2] == "svm") predictions <- attr(predictions,"probabilities")[,"1"]
  
     
  if (is.matrix(predictions)) if (ncol(predictions) == 2 ) predictions <- predictions[,2]
  perf[i] <- AUC::auc(roc(predictions,ytest))
  }
  perf <- data.frame(grid, auc=perf)
  perf[which.max(perf$auc),]
}